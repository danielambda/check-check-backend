{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Configuration.Dotenv (loadFile, defaultConfig)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
  (cors, simpleCorsResourcePolicy, corsMethods, corsOrigins, corsRequestHeaders)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, close)
import Data.ByteString.Char8 as B (pack)
import Data.Pool (Pool, newPool, defaultPoolConfig, destroyAllResources)

import System.Environment (getEnv)
import Control.Exception (finally)

import WebAPI (application)
import WebAPI.AppM (Env(..))

main :: IO ()
main = do
  loadFile defaultConfig

  connStr <- B.pack <$> getEnv "POSTGRESQL_CONNECTION_STRING"
  pool <- newPool $ defaultPoolConfig
    (connectPostgreSQL connStr)
    close
    20.0   -- Keepalive time (seconds)
    50     -- Max resourcesction is kept

  runAPI pool
    `finally` destroyAllResources pool

runAPI :: Pool Connection -> IO ()
runAPI conn = do
  putStrLn $ "   Hello! The app is running on port " <> show port
  run port $ corsPolicy $ application env
    where
      port = 8080
      env = Env
        { envConnPool = conn
        , envPort = port
        }
      corsPolicy = cors $ const $ Just $ simpleCorsResourcePolicy
        { corsOrigins = Just (["http://localhost:3000"], True)
        , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
        , corsRequestHeaders = ["Authorization", "Content-Type"]
        }
