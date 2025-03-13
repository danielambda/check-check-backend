{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Servant.Auth.Client (Token)
import Telegram.Bot.API
  ( BotName(..), Update, defaultTelegramClientEnv , userUsername
  , responseResult, SomeReplyMarkup (SomeInlineKeyboardMarkup), InlineKeyboardMarkup (..)
  , updateChatId
  )
import Telegram.Bot.Simple
  ( BotApp(..), Eff, startBot_, getEnvToken, ReplyMessage (replyMessageReplyMarkup), reply
  , actionButton, conversationBot, replyText
  , editUpdateMessage, EditMessage (..), BotM, GetAction, withEffect, BotContext (botContextUser), toReplyMessage
  )
import Telegram.Bot.Simple.UpdateParser
  ( parseUpdate, command, commandWithBotName, callbackQueryDataRead )
import Configuration.Dotenv (loadFile, defaultConfig)
import Servant.Client (runClientM, ClientM, mkClientEnv, Scheme (Http), BaseUrl (BaseUrl), ClientError, ClientEnv)

import qualified Data.Text as T

import CheckCheck.Contracts.Receipts (ReceiptResp (..), ReceiptItemResp (..))
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT), MonadReader (ask), asks)
import Control.Monad (unless)
import Data.Functor ((<&>))
import Data.Text (Text)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Optics (LabelOptic (labelOptic), A_Getter, to, unsafeFiltered, (%), (&), traversed, _1, _2, (.~), (^.), view)
import qualified Telegram.Bot.API as TG
import SmartPrimitives.Positive (pattern Positive)
import Clients
  ( ApiClient(..), apiClient
  , UsersClient (..)
  , OutgointRequestsClient (..), ContactsClient (ContactsClient, getContacts)
  )
import Data.UUID (UUID, fromString, toString)
import qualified AuthServiceClient as Auth (getJwtToken)
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.Text.Encoding (encodeUtf16BE)
import CheckCheck.Contracts.Users.OutgoingRequests (SendRequestReqBody(..), IndexSelectionReqBody (..))
import Data.List.NonEmpty (NonEmpty, nonEmpty, toList, singleton)
import Data.Maybe (fromJust)
import SmartPrimitives.TextMaxLen (TextMaxLen)
import CheckCheck.Contracts.Users.Contacts (ContactResp(..))

currentUser :: AppM TG.User
currentUser = AppM $ ReaderT $ const $
  asks botContextUser

data ReceiptItem = ReceiptItem
  { index :: Int
  , name :: Text
  , price :: Integer
  , quantity :: Double
  } deriving (Show, Read)

data UserContact = UserContact
  { contactUserId :: UUID
  , mContactName :: Maybe (TextMaxLen 50)
  }

instance LabelOptic "index" A_Getter ReceiptItem ReceiptItem Int Int where
  labelOptic = to $ \ReceiptItem{ index } -> index
instance LabelOptic "name" A_Getter ReceiptItem ReceiptItem Text Text where
  labelOptic = to $ \ReceiptItem{ name } -> name

data State
  = InitialState
  | SelectingReceiptItems Text [(Bool, ReceiptItem)]
  | SelectingRequestRecipient Text (NonEmpty Int)

data Transition
  = Id
  | ShowReceipt Text
  | StartSelectingReceiptItems Text [ReceiptItem]
  | SelectReceiptItem Int
  | StartSelectingRequestRecipient
  | SelectRequestRecipient UUID
  deriving (Read, Show)

mkBotApp :: ClientEnv -> ClientEnv -> ByteString -> BotName -> BotApp State Transition
mkBotApp clientEnv authClientEnv secret botName = BotApp
  { botInitialModel = InitialState
  , botAction = flip $ decideTransition botName
  , botHandler = botHandler
  , botJobs = []
  }
  where
    botHandler action model = nt $ handleTransition action model
    nt :: Eff' action model -> Eff action model
    nt = flip runReaderT Env{..}

data Env = Env
  { clientEnv :: ClientEnv
  , authClientEnv :: ClientEnv
  , secret :: ByteString
  }

newtype AppM a = AppM { _runAppM :: ReaderT Env BotM a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

runReq :: ClientM a -> AppM (Either ClientError a)
runReq req = do
  clientEnv <- asks clientEnv
  liftIO $ runClientM req clientEnv

runAuthReq :: ClientM a -> AppM (Either ClientError a)
runAuthReq req = do
  authClientEnv <- asks authClientEnv
  liftIO $ runClientM req authClientEnv

tg :: BotM a -> AppM a
tg = AppM . ReaderT . const

type Eff' action = ReaderT Env (Eff action)

tshow :: Show a => a -> Text
tshow = T.pack . show

infix 0 <#
(<#) :: GetAction a action => state -> AppM a -> Eff' action state
state <# AppM app = do
  env <- ask
  let bot = runReaderT app env
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

getJwtToken :: TG.User -> AppM Token
getJwtToken user = do
  secret <- asks secret
  runAuthReq (Auth.getJwtToken secret user) >>= \case
    Left _ -> getJwtToken user
    Right (Right token) -> return token
    Right (Left err) -> error $ "jose error" <> show err

decideTransition :: BotName -> State -> Update -> Maybe Transition
decideTransition (BotName botName) state = parseUpdate parser
  where
    command' = liftA2 (<|>) command (commandWithBotName botName)

    parser = case state of
      InitialState -> ShowReceipt <$> (command' "qr" <|> command' "receipt")

      SelectingReceiptItems{} -> do
        transition <- callbackQueryDataRead
        unless (isAllowed transition) $
          fail "unsupported transition"
        return transition
        where
          isAllowed SelectReceiptItem{} = True
          isAllowed StartSelectingRequestRecipient{} = True
          isAllowed _ = False

      SelectingRequestRecipient{} -> do
        transition <- callbackQueryDataRead
        unless (isAllowed transition) $
          fail "unsupported transition"
        return transition
        where
          isAllowed SelectRequestRecipient{} = True
          isAllowed _ = False

handleTransition :: Transition -> State -> Eff' Transition State
handleTransition transition InitialState = case transition of
  ShowReceipt qr -> InitialState <# do
    let ApiClient{ getReceipt } = apiClient
    runReq (getReceipt qr) >>= \case
      Left err -> do
        liftIO $ putStrLn $ "Error: " <> show err
        return Id
      Right (ReceiptResp items) -> do
        let items' = process items
        let confirmButton = actionButton "Confirm" StartSelectingRequestRecipient
        let itemsButtons = items'
              <&> (:[])
               .  \(index, item) -> actionButton item (SelectReceiptItem index)
        let buttons = itemsButtons ++ [[confirmButton]]
        let msg' = "Scanned receipt items: "
              { replyMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup $ InlineKeyboardMarkup
                { inlineKeyboardMarkupInlineKeyboard = buttons }
              }
        tg $ reply msg'
        return $ StartSelectingReceiptItems qr $ items
          <&> \ReceiptItemResp{ price = Positive price, quantity = Positive quantity, .. } ->
            ReceiptItem{..}
  StartSelectingReceiptItems qr items -> pure $ SelectingReceiptItems qr (map (False,) items)
  _ -> undefined

handleTransition transition (SelectingReceiptItems qr items) = case transition of
  SelectReceiptItem i ->
    let items' = items & traversed % unsafeFiltered ((i ==) . (^. _2 % #index)) % _1 .~ True
    in SelectingReceiptItems qr items' <# do
      let confirmButton = actionButton "Confirm" StartSelectingRequestRecipient
      let itemsButtons = items'
            <&> (:[])
             .  \(_, ReceiptItem{ index, name }) -> actionButton name (SelectReceiptItem index)
      let buttons = itemsButtons ++ [[confirmButton]]
      let editMessage' = EditMessage
            { editMessageText = "selected items: " <> tshow (view #index . snd <$> filter fst items')
            , editMessageParseMode = Nothing
            , editMessageLinkPreviewOptions = Nothing
            , editMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup $ InlineKeyboardMarkup
              { inlineKeyboardMarkupInlineKeyboard = buttons }
            }
      tg $ editUpdateMessage editMessage'

  StartSelectingRequestRecipient ->
    case nonEmpty $ items & filter fst & map snd of
      Just filteredItems -> SelectingRequestRecipient qr (view #index <$> filteredItems) <# do
        let overallSum = (`divide` 100) $ sum $ filteredItems <&>
              \ReceiptItem{ quantity, price } ->
                round (fromIntegral price * quantity)
        let replyMsgText = T.unlines
              $ ("В сумме на: " <> tshow overallSum)
              : (view #name <$> toList filteredItems)
        token <- getJwtToken =<< currentUser
        let ContactsClient{ getContacts } = contactsClient $ mkUsersClient apiClient token
        runReq getContacts >>= \case
          Left err -> liftIO $ print err
          Right contactsResp -> do
            let contacts = contactsResp <&>
                  \ContactResp{..} -> UserContact{ mContactName = contactName, ..}
            let replyMsg = (toReplyMessage replyMsgText)
                  { replyMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup $ InlineKeyboardMarkup
                    { inlineKeyboardMarkupInlineKeyboard = [[
                      actionButton "send to aboba" $ SelectRequestRecipient $ fromJust $ fromString "1e8dd476-f769-48b4-8af7-58fd5095bc06"
                    ]] }
                  }

            tg $ reply replyMsg

      Nothing -> SelectingReceiptItems qr items <# do
        tg $ replyText "this text has to be edited, btw you did not select anything"
  _ -> undefined

handleTransition transition (SelectingRequestRecipient qr indices) = case transition of
  SelectRequestRecipient recipientId -> SelectingRequestRecipient qr indices <# do
    token <- getJwtToken =<< currentUser
    let OutgointRequestsClient{ sendRequest } = outgoingRequestsClient $ mkUsersClient apiClient token
    let reqBody = SendReceiptItemsRequestReqBody
          { receiptQr = qr
          , indexSelections = singleton $
            IndexSelectionReqBody{..}
          }
    runReq (sendRequest reqBody) >>= \case
      Right _ -> tg $ replyText $ T.pack $ toString recipientId
      Left err -> liftIO $ print err
    return ()
  _ -> undefined

run :: TG.Token -> IO ()
run token = do
  tgEnv <- defaultTelegramClientEnv token
  mBotName <- either (error . show) (userUsername . responseResult) <$> runClientM TG.getMe tgEnv
  let botName = maybe (error "bot name is not defined") BotName mBotName

  manager <- newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager (BaseUrl Http "localhost" 8080 "")
  let authClientEnv = mkClientEnv manager (BaseUrl Http "localhost" 5183 "")

  let TG.Token secret' = token
  let secret = fromStrict $ encodeUtf16BE secret'
  let botApp = conversationBot updateChatId $ mkBotApp clientEnv authClientEnv secret botName
  startBot_ botApp tgEnv


main :: IO ()
main = do
  loadFile defaultConfig

  putStrLn "The bot is running"
  run =<< getEnvToken "TELEGRAM_BOT_TOKEN"

