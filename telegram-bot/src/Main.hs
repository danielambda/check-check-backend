{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Telegram.Bot.API
  ( BotName(..), Update, defaultTelegramClientEnv , userUsername
  , responseResult, SomeReplyMarkup (SomeInlineKeyboardMarkup), InlineKeyboardMarkup (..)
  , updateChatId
  )
import Telegram.Bot.Simple
  ( BotApp(..), Eff, startBot_, getEnvToken, ReplyMessage (replyMessageReplyMarkup), reply
  , actionButton, conversationBot, replyText
  , editUpdateMessage, EditMessage (..), BotM, GetAction, withEffect
  )
import Telegram.Bot.Simple.UpdateParser
  ( parseUpdate, command, commandWithBotName, callbackQueryDataRead )
import Configuration.Dotenv (loadFile, defaultConfig)
import Servant.Client (runClientM, ClientM, client, mkClientEnv, Scheme (Http), BaseUrl (BaseUrl), ClientError, ClientEnv)

import qualified Data.Text as T

import CheckCheck.Contracts.Receipts (ReceiptResp (..), ReceiptItemResp (..))
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT), MonadReader (ask))
import Control.Monad (unless)
import Data.Text (Text)
import Data.Functor ((<&>))
import Data.Proxy (Proxy(..))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import SmartPrimitives.Positive (pattern Positive)
import CheckCheck.Contracts.API (API)
import Servant.API ((:<|>)(..))
import Servant.Auth.Client (Token)
import CheckCheck.Contracts.Groups (CreateGroupReqBody, GroupResp)
import Data.UUID (UUID)
import qualified Telegram.Bot.API as TG
import CheckCheck.Contracts.Users (UserResp)

data ApiClient = ApiClient
  { getReceipt :: Text -> ClientM ReceiptResp
  , mkGroupsClient :: Token -> GroupsClient
  , mkUsersClient :: Token -> UsersClient
  }

data GroupsClient = GroupsClient
  { createGroup :: CreateGroupReqBody -> ClientM GroupResp
  , getGroup :: UUID -> ClientM GroupResp
  , getAllGroups :: ClientM [GroupResp]
  }

data UsersClient = UsersClient
  { getMe :: ClientM UserResp
  , aboba :: Int
  }

mkApiClient :: ApiClient
mkApiClient = ApiClient{..}
  where
    getReceipt :<|> groupsClient :<|> usersClient = client $ Proxy @API

    mkGroupsClient token = GroupsClient{..}
      where
        createGroup :<|> getGroup :<|> getAllGroups :<|> _ = groupsClient token

    mkUsersClient token = UsersClient{..}
      where
        getMe :<|> _ = usersClient token

data State
  = InitialState
  | SelectingReceiptItems Text [Int]

data Transition
  = ShowReceipt Text
  | SelectReceiptItem Int
  | FinishSelectingReceiptItems
  deriving (Read, Show)

mkBotApp :: ClientEnv -> BotName -> BotApp State Transition
mkBotApp clientEnv botName = BotApp
  { botInitialModel = InitialState
  , botAction = flip $ decideTransition botName
  , botHandler = botHandler
  , botJobs = []
  }
  where
    botHandler action model = nt clientEnv $ handleTransition action model
    nt :: ClientEnv -> Eff' action model -> Eff action model
    nt = flip runReaderT

newtype AppM a = AppM { _runAppM :: ReaderT ClientEnv BotM a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader ClientEnv)

runReq :: ClientM a -> AppM (Either ClientError a)
runReq req = do
  clientEnv <- ask
  liftIO $ runClientM req clientEnv

tg :: BotM a -> AppM a
tg = AppM . ReaderT . const

type Eff' action = ReaderT ClientEnv (Eff action)

tshow :: Show a => a -> Text
tshow = T.pack . show

(<#) :: GetAction a action => state -> AppM a -> Eff' action state
state <# (AppM app) = do
  clientEnv <- ask
  let bot = runReaderT app clientEnv
  ReaderT $ const $ withEffect bot state

divide :: Int -> Double -> Double
divide a b = fromIntegral a / b

process :: [ReceiptItemResp] -> [(Int, Text)]
process = fmap
  \ReceiptItemResp{ index, name, quantity = Positive quantity, price = Positive price } ->
    ( index
    , tshow index
      <> ". "
      <> T.take 20 name
      <> "... x "
      <> tshow quantity
      <> " = "
      <> tshow (round (fromIntegral price * quantity) `divide` 100)
      <> " rub"
    )

decideTransition :: BotName -> State -> Update -> Maybe Transition
decideTransition (BotName botName) state = parseUpdate parser
  where
    command' = liftA2 (<|>) command (commandWithBotName botName)

    parser = case state of
      InitialState -> ShowReceipt <$> (command' "qr" <|> command' "receipt")

      SelectingReceiptItems _ _ -> do
        transition <- callbackQueryDataRead
        unless (isAllowed transition) $
          fail "unsupported transition"
        return transition
          where
            isAllowed (SelectReceiptItem _) = True
            isAllowed FinishSelectingReceiptItems = True
            isAllowed _ = False

handleTransition :: Transition -> State -> Eff' Transition State
handleTransition transition InitialState = case transition of
  ShowReceipt qr -> SelectingReceiptItems qr [] <# do
    let ApiClient{ getReceipt } = mkApiClient
    runReq (getReceipt qr) >>= \case
      Left err -> liftIO $ putStrLn $ "Error: " <> show err
      Right (ReceiptResp items) -> do
        let items' = process items
        let confirmButton = actionButton "Confirm" FinishSelectingReceiptItems
        let itemsButtons = items'
              <&> (: [])
               .  \(index, item) -> actionButton item (SelectReceiptItem index)
        let buttons = itemsButtons ++ [[confirmButton]]
        let msg' = "Scanned receipt items: "
              { replyMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup $ InlineKeyboardMarkup
                { inlineKeyboardMarkupInlineKeyboard = buttons }
              }
        tg $ reply msg'
  _ -> undefined

handleTransition action (SelectingReceiptItems qr indices) = case action of
  SelectReceiptItem index ->
    let indices' = index:indices in
    SelectingReceiptItems qr indices' <# do
    let ApiClient{ getReceipt } = mkApiClient
    runReq (getReceipt qr) >>= \case
      Left err -> liftIO $ putStrLn $ "Error: " <> show err
      Right (ReceiptResp items) -> do
        let items' = process items
        let confirmButton = actionButton "Confirm" FinishSelectingReceiptItems
        let itemsButtons = items'
              <&> (: [])
               .  \(i, item) -> actionButton item (SelectReceiptItem i)
        let buttons = itemsButtons ++ [[confirmButton]]
        let editMessage' = EditMessage
              { editMessageText = "selected items: " <> tshow indices'
              , editMessageParseMode = Nothing
              , editMessageLinkPreviewOptions = Nothing
              , editMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup $ InlineKeyboardMarkup
                { inlineKeyboardMarkupInlineKeyboard = buttons }
              }
        tg $ editUpdateMessage editMessage'
  FinishSelectingReceiptItems -> InitialState <# do
    let ApiClient{ getReceipt } = mkApiClient
    runReq (getReceipt qr) >>= \case
      Left err -> liftIO $ putStrLn $ "Error: " <> show err
      Right (ReceiptResp items) -> do
        let filteredItems = filter (flip elem indices . index) items
        let overallSum = (`divide` 100) $ sum $ filteredItems <&>
              \ReceiptItemResp{ quantity = Positive quantity, price = Positive price } ->
                round (fromIntegral price * quantity)
        let msgLines
              = ("В сумме на: " <> tshow overallSum)
              : (snd <$> process filteredItems)
        tg $ replyText $ T.unlines msgLines
  _ -> undefined

run :: TG.Token -> IO ()
run token = do
  tgEnv <- defaultTelegramClientEnv token
  mBotName <- either (error . show) (userUsername . responseResult) <$> runClientM TG.getMe tgEnv
  let botName = maybe (error "bot name is not defined") BotName mBotName

  manager <- newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager (BaseUrl Http "localhost" 8080 "")

  let botApp = conversationBot updateChatId $ mkBotApp clientEnv botName
  startBot_ botApp tgEnv

main :: IO ()
main = do
  loadFile defaultConfig

  putStrLn "The bot is running"
  run =<< getEnvToken "TELEGRAM_BOT_TOKEN"

