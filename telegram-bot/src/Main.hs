{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Telegram.Bot.API
  ( BotName(..), Update, Token, defaultTelegramClientEnv, userUsername, responseResult, getMe )
import Telegram.Bot.Simple (BotApp(..), Eff, startBot_, getEnvToken, (<#), replyText)
import Telegram.Bot.Simple.UpdateParser (parseUpdate, command, commandWithBotName)
import Configuration.Dotenv (loadFile, defaultConfig)
import Servant.Client (runClientM, ClientM, client, mkClientEnv, Scheme (Http), BaseUrl (BaseUrl))

import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as T

import CheckCheck.Contracts.Receipts (ReceiptsAPI, ReceiptResp (ReceiptResp), ReceiptItemResp (..))
import Data.Proxy (Proxy(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Functor ((<&>))
import SmartPrimitives.Positive (pattern Positive)
import Network.HTTP.Client (defaultManagerSettings, newManager)

getReceipt :: Text -> ClientM ReceiptResp
getReceipt = client $ Proxy @ReceiptsAPI

newtype Model = Model
  { botName :: BotName }
  deriving Show

data Action
  = NoAction
  | ShowReceiptQr Text
  deriving Show

bot :: BotName -> BotApp Model Action
bot botName = BotApp
  { botInitialModel = Model botName
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }

handleUpdate :: Model -> Update -> Maybe Action
handleUpdate Model{ botName = BotName botName } = parseUpdate
  $ ShowReceiptQr <$> (command' "qr" <|> command' "receipt")
  where
    command' =
       liftA2 (<|>) command (commandWithBotName botName)

handleAction :: Action -> Model -> Eff Action Model
handleAction NoAction model = pure model

handleAction (ShowReceiptQr qr) model = model <# do
  manager <- liftIO $ newManager defaultManagerSettings
  result <- liftIO $ runClientM (getReceipt qr) (mkClientEnv manager (BaseUrl Http "localhost" 8080 "receipts"))
  case result of
    Left err -> liftIO $ putStrLn $ "Error: " <> show err
    Right (ReceiptResp items) -> do
      let msg = T.unlines $ items <&>
            \ReceiptItemResp{ name, quantity = Positive quantity, price = Positive price }
              -> name
              <> " в количестве "
              <> T.pack (show quantity)
              <> "; в сумме на "
              <> T.pack (show $ fromIntegral price * quantity / 100)
              <> " рублей"
      replyText msg
  return NoAction

run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  mUsername <- either (error . show) (userUsername . responseResult) <$> runClientM getMe env
  let botName = maybe (error "bot name is not defined") BotName mUsername
  startBot_ (bot botName) env

main :: IO ()
main = do
  loadFile defaultConfig

  putStrLn "The bot is running"
  run =<< getEnvToken "TELEGRAM_BOT_TOKEN"

