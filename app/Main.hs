{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Configuration.Dotenv (loadFile, defaultConfig)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
  (cors, simpleCorsResourcePolicy, corsMethods, corsOrigins, corsRequestHeaders)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import Data.ByteString.Char8 as B (pack)

import System.Environment (getEnv, getArgs)

import InitDb (initDb)
import App (Env(..), application)
import Receipts (initReceiptsEnv, ReceiptsEnv)
import Control.Monad.Reader (ReaderT(runReaderT))

main :: IO ()
main = do
  loadFile defaultConfig
  conn <- connectPostgreSQL . B.pack =<< getEnv "POSTGRESQL_CONNECTION_STRING"
  args <- getArgs

  if "initdb" `elem` args then
    runReaderT initDb conn
  else do
    receiptsEnv <- initReceiptsEnv
    runAPI conn receiptsEnv

runAPI :: Connection -> ReceiptsEnv -> IO ()
runAPI conn receiptsEnv = do
  putStrLn $ "   Hello! The app is running on port " <> show port
  run port $ corsPolicy $ application env
    where
      port = 8080
      env = Env
        { envConnPool = conn
        , envPort = port
        , envReceiptsEnv = receiptsEnv
        }
      corsPolicy = cors $ const $ Just $ simpleCorsResourcePolicy
        { corsOrigins = Just (["http://localhost:3000"], True)
        , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
        , corsRequestHeaders = ["Authorization", "Content-Type"]
        }
