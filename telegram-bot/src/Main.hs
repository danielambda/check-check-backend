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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import Servant.Auth.Client (Token)
import Telegram.Bot.API
  ( BotName(..), Update (updateMessage), defaultTelegramClientEnv , userUsername
  , responseResult, SomeReplyMarkup (SomeInlineKeyboardMarkup), InlineKeyboardMarkup (..)
  , updateChatId, Message (messageFrom)
  )
import Telegram.Bot.Simple
  ( BotApp(..), Eff, startBot_, getEnvToken, ReplyMessage (replyMessageReplyMarkup), reply
  , actionButton, conversationBot, replyText
  , editUpdateMessage, EditMessage (..), BotM, GetAction, withEffect, BotContext (botContextUpdate), toReplyMessage
  )
import Telegram.Bot.Simple.UpdateParser
  ( parseUpdate, command, commandWithBotName, callbackQueryDataRead )
import Configuration.Dotenv (loadFile, defaultConfig)
import Servant.Client (runClientM, mkClientEnv, Scheme (Http), BaseUrl (BaseUrl), ClientError, ClientEnv, matchUnion)

import qualified Data.Text as T

import CheckCheck.Contracts.Receipts (ReceiptResp (..), ReceiptItemResp (..))
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT), MonadReader (ask), asks)
import Control.Monad (unless, forM, (>=>))
import Data.Functor ((<&>))
import Data.Text (Text)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Optics (LabelOptic (labelOptic), A_Getter, to, unsafeFiltered, (%), (&), traversed, _1, _2, (.~), (^.), view)
import qualified Telegram.Bot.API as TG
import SmartPrimitives.Positive (pattern Positive)
import Clients
  ( ApiClient(..), apiClient
  , UsersClient(..)
  , OutgointRequestsClient(..), ContactsClient(..)
  )
import ClientMUtils
  ( runReq, runReq_, HasKeyedClientEnv(..), FromClientError(..), FromClientError )
import CheckCheck.Contracts.Users.Contacts (ContactResp(..), CreateContactReqBody (..))
import CheckCheck.Contracts.Users.OutgoingRequests (SendRequestReqBody(..), IndexSelectionReqBody (..))
import Data.List.NonEmpty (NonEmpty, nonEmpty, toList, singleton)
import Data.UUID (UUID, toString)
import AuthServiceClient
    ( authTelegram, UserQuery(..), getUser, AuthServiceUser(..) )
import SmartPrimitives.TextMaxLen (TextMaxLen, unTextMaxLen, mkTextMaxLen)
import System.Environment (getEnv)
import Servant.API (WithStatus)
import CheckCheck.Contracts.Users (UserResp(..))
import SmartPrimitives.TextLenRange (unTextLenRange)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)

currentUser :: AppM (Maybe TG.User)
currentUser = AppM $ ReaderT $ const $
  asks $ botContextUpdate >=> updateMessage >=> messageFrom

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
  deriving Show

data Transition
  = GetCurrentState
  | Id
  | Start
  | ShowReceipt Text
  | AddContact Text
  | StartSelectingReceiptItems Text [ReceiptItem]
  | SelectReceiptItem Int
  | StartSelectingRequestRecipient
  | SelectRequestRecipient UUID
  deriving (Read, Show)

mkBotApp :: ClientEnv -> ClientEnv -> String -> BotName -> BotApp State Transition
mkBotApp backendClientEnv authClientEnv secret botName = BotApp
  { botInitialModel = InitialState
  , botAction = flip $ decideTransition botName
  , botHandler = botHandler
  , botJobs = []
  } where
    botHandler = fmap nt . handleTransition
    nt :: Eff' transition state -> Eff transition state
    nt = flip runReaderT Env{..}

newtype AppM a = AppM { _unAppM :: ReaderT Env (ExceptT AppError BotM) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadError AppError)

newtype AppError = AppClientError ClientError
  deriving (Show)

instance FromClientError AppError where
  fromClientError = AppClientError

data Env = Env
  { backendClientEnv :: ClientEnv
  , authClientEnv :: ClientEnv
  , secret :: String
  }

instance HasKeyedClientEnv Env "backend" where
  getClientEnv _ = backendClientEnv

instance HasKeyedClientEnv Env "auth" where
  getClientEnv _ = authClientEnv

tg :: BotM a -> AppM a
tg = AppM
   . ReaderT . const
   . ExceptT . fmap Right

type Eff' action = ReaderT Env (Eff action)

tshow :: Show a => a -> Text
tshow = T.pack . show

infix 0 <#
(<#) :: GetAction a action => state -> AppM a -> Eff' action state
state <# AppM app = do
  env <- ask
  let bot = runExceptT (runReaderT app env) >>= \case
        Right a -> return a
        Left err -> error $ show err
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

authViaTelegram :: TG.User -> AppM Token
authViaTelegram user = do
  secret <- asks secret
  runReq $ authTelegram secret user

decideTransition :: BotName -> State -> Update -> Maybe Transition
decideTransition (BotName botName) state = parseUpdate $
  parser <|> GetCurrentState <$ command' "state"
  where
    command' cmd = command cmd <|> commandWithBotName botName cmd

    parser = case state of
      InitialState
         -> ShowReceipt <$> (command' "qr" <|> command' "receipt")
        <|> AddContact <$> command' "contact"
        <|> Start <$ command' "start"

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
handleTransition GetCurrentState state = state <# do tg $ replyText $ tshow state
handleTransition Id state = pure state
handleTransition transition InitialState = case transition of
  Start -> InitialState <# do
    currentUser >>= \case
      Nothing -> return ()
      Just user -> do
        token <- authViaTelegram user
        let UsersClient{ getMe }  = mkUsersClient apiClient token
        UserResp{ username } <- runReq getMe
        tg $ replyText $ "Nice to see you, " <> unTextLenRange username

  ShowReceipt qr -> InitialState <# do
    let ApiClient{ getReceipt } = apiClient
    ReceiptResp items <- runReq (getReceipt qr)
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

  AddContact content -> InitialState <# currentUser >>= \case
    Nothing -> return ()
    Just user -> authViaTelegram user >>= \token ->
      case T.uncons content of
      Nothing -> do
        tg $ replyText "Please, enter non empty username to add to contacts"
      Just ('@', content') -> case T.span (== ' ') content' of
        (contactTgUsername, "") -> do
          u <- runReq $ getUser token (UserTgUsernameQuery contactTgUsername)
          if | Just AuthServiceUser{ userId } <- matchUnion @AuthServiceUser u -> do
              let ContactsClient{ createContact } = contactsClient $ mkUsersClient apiClient token
              let reqBody = CreateContactReqBody
                    { contactUserId = userId
                    , contactName = Nothing
                    }
              runReq_ $ createContact reqBody
              tg $ replyText $ "contact @" <> contactTgUsername <> " successfully added"
             | Just _ <- matchUnion @(WithStatus 404 Text) u -> do
              tg $ replyText $ T.unlines
                [ "User @" <> contactTgUsername <> " is not registered in check-check"
                , "Send them the following link to join:"
                ]
              tg $ replyText "https://t.me/CheckCheckTgBot?start=start" -- TODO remove the hardlink
              | otherwise -> error "unreachable"
        (contactTgUsername, mContactName) -> case mkTextMaxLen mContactName of
          Nothing -> tg $ replyText $
            "contact name " <> mContactName <> " is too long, 50 symbols is the max length"
          Just contactName -> do
            u <- runReq $ getUser token (UserTgUsernameQuery contactTgUsername)
            if | Just AuthServiceUser{ userId } <- matchUnion @AuthServiceUser u -> do
                let ContactsClient{ createContact } = contactsClient $ mkUsersClient apiClient token
                let reqBody = CreateContactReqBody
                      { contactUserId = userId
                      , contactName = Just contactName
                      }
                runReq_ (createContact reqBody)
                tg $ replyText $ "contact @" <> contactTgUsername <> " successfully added as " <> mContactName
               | Just _ <- matchUnion @(WithStatus 404 Text) u -> do
                tg $ replyText $ T.unlines
                  [ "User @" <> contactTgUsername <> " is not registered in check-check"
                  , "Send them the following link to join:"
                  ]
                tg $ replyText "https://t.me/CheckCheckTgBot?start=start" -- TODO remove the hardlink
               | otherwise -> error "unreachable"
      _ -> case T.span (== ' ') content of
        (contactUsername, "") -> do
          u <- runReq $ getUser token (UserUsernameQuery contactUsername)
          if | Just AuthServiceUser{ userId } <- matchUnion @AuthServiceUser u -> do
              let ContactsClient{ createContact } = contactsClient $ mkUsersClient apiClient token
              let reqBody = CreateContactReqBody
                    { contactUserId = userId
                    , contactName = Nothing
                    }
              runReq_ (createContact reqBody)
              tg $ replyText $
                  "contact " <> contactUsername <> " successfully added"
              | Just _ <- matchUnion @(WithStatus 404 Text) u -> do
              tg $ replyText $
                "User " <> contactUsername <> " is not registered in check-check"
              | otherwise -> error "unreachable"

        (contactUsername, mContactName) -> case mkTextMaxLen mContactName of
          Nothing -> tg $ replyText $
            "contact name " <> mContactName <> " is too long, 50 symbols is the max length"
          Just contactName -> do
            u <- runReq $ getUser token (UserUsernameQuery contactUsername)
            if | Just AuthServiceUser{ userId } <- matchUnion @AuthServiceUser u -> do
                let ContactsClient{ createContact } = contactsClient $ mkUsersClient apiClient token
                let reqBody = CreateContactReqBody
                      { contactUserId = userId
                      , contactName = Just contactName
                      }
                runReq_ (createContact reqBody)
                tg $ replyText $
                    "contact " <> contactUsername <> " successfully added as " <> mContactName
               | Just _ <- matchUnion @(WithStatus 404 Text) u -> do
                tg $ replyText $
                  "User " <> contactUsername <> " is not registered in check-check"
               | otherwise -> error "unreachable"

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
        currentUser >>= \case
          Nothing -> return ()
          Just user -> do
            token <- authViaTelegram user
            let ContactsClient{ getContacts } = contactsClient $ mkUsersClient apiClient token
            contactsResp <- runReq getContacts
            let contacts = contactsResp <&>
                  \ContactResp{..} -> UserContact{ mContactName = contactName, ..}
            buttons <- forM contacts $ \UserContact{..} -> do
              let name = maybe "aboba" unTextMaxLen mContactName
              -- TODO pull usernames for contacts without contact name
              return $ actionButton name (SelectRequestRecipient contactUserId)
            let replyMsg = (toReplyMessage replyMsgText)
                  { replyMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup $ InlineKeyboardMarkup
                    { inlineKeyboardMarkupInlineKeyboard = buttons <&> (:[]) }
                  }
            tg $ reply replyMsg

      Nothing -> SelectingReceiptItems qr items <# do
        tg $ replyText "this text has to be edited, btw you did not select anything"
  _ -> undefined

handleTransition transition (SelectingRequestRecipient qr indices) = case transition of
  SelectRequestRecipient recipientId -> InitialState <# do
    currentUser >>= \case
      Nothing -> return ()
      Just user -> do
        token <- authViaTelegram user
        let OutgointRequestsClient{ sendRequest } = outgoingRequestsClient $ mkUsersClient apiClient token
        let reqBody = SendReceiptItemsRequestReqBody
              { receiptQr = qr
              , indexSelections = singleton $ IndexSelectionReqBody{..}
              }
        runReq_ $ sendRequest reqBody
        tg $ replyText $ T.pack $ toString recipientId
        return ()
  _ -> undefined

run :: String -> TG.Token -> IO ()
run authApiKey token = do
  tgEnv <- defaultTelegramClientEnv token
  mBotName <- either (error . show) (userUsername . responseResult) <$> runClientM TG.getMe tgEnv
  let botName = maybe (error "bot name is not defined") BotName mBotName

  manager <- newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager (BaseUrl Http "localhost" 8080 "")
  let authClientEnv = mkClientEnv manager (BaseUrl Http "localhost" 5183 "")

  let botApp = conversationBot updateChatId $ mkBotApp clientEnv authClientEnv authApiKey botName
  startBot_ botApp tgEnv

main :: IO ()
main = do
  loadFile defaultConfig

  putStrLn "The bot is running"
  authApiKey <- getEnv "AUTH_API_KEY"
  token <- getEnvToken "TELEGRAM_BOT_TOKEN"
  run authApiKey token

