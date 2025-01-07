{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Configuration.Dotenv (loadFile, defaultConfig)
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import Servant
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Control.Monad.Reader (ReaderT (runReaderT), asks)
import Data.ByteString.Char8 as B (pack)

import System.Environment (getEnv)

import Receipts
import Common.Persistence (MonadConnPoolReader, askConnPool)

type API = ReceiptsAPI

type AppM = ReaderT Env Handler
data Env = Env
  { envConnPool :: Connection
  , envPort :: Int
  , envReceiptsEnv :: ReceiptsEnv
  }

instance MonadReceiptsEnvReader AppM where
  askReceiptsEnv = asks envReceiptsEnv
instance MonadConnPoolReader AppM where
  askConnPool = asks envConnPool

application :: Env -> Application
application env =
  serve api $ hoistServer api nt server
  where
    api :: Proxy API; api = Proxy

    nt :: AppM a -> Handler a
    nt = flip runReaderT env

server :: ServerT API AppM
server = receiptsServer

corsPolicy :: Middleware
corsPolicy = cors (const $ Just policy)
  where
    policy = simpleCorsResourcePolicy
      { corsOrigins = Just (["http://localhost:3000"], True)
      , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
      , corsRequestHeaders = ["Authorization", "Content-Type"]
      }

main :: IO ()
main = do
  loadFile defaultConfig

  conn <- connectPostgreSQL . B.pack =<< getEnv "POSTGRESQL_CONNECTION_STRING"
  receiptsEnv <- initReceiptsEnv
  let env = Env
          { envConnPool = conn
          , envPort = 8080
          , envReceiptsEnv = receiptsEnv
          }

  let port = 8080
  putStrLn $ "   Hello! The app is running on port " <> show port
  run port $ corsPolicy $ application env
  close conn
