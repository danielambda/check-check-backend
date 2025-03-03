{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Configuration.Dotenv (loadFile, defaultConfig)
import Network.Wai.Handler.Warp
  (defaultSettings, setPort, setBeforeMainLoop, runSettings)
import Network.Wai.Middleware.Cors
  (cors, simpleCorsResourcePolicy, corsMethods, corsOrigins, corsRequestHeaders)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, close)
import Data.ByteString.Char8 as B (pack)
import Data.Pool (Pool, newPool, defaultPoolConfig, destroyAllResources)

import System.IO (hPutStrLn, stderr)
import System.Environment (getEnv)
import Control.Exception (finally)
import Data.Function ((&))

import WebAPI.AppM (Env(Env))
import WebAPI (mkApp)

main :: IO ()
main = do
  loadFile defaultConfig

  conn <- connectPostgreSQL . B.pack <$> getEnv "POSTGRESQL_CONNECTION_STRING"
  pool <- newPool $ defaultPoolConfig conn close
                      20 -- Keepalive time (seconds)
                      50 -- Max resourcesction is kept
  runAPI pool
    `finally` destroyAllResources pool

runAPI :: Pool Connection -> IO ()
runAPI connPool = do
  app <- corsPolicy <$> mkApp env
  runSettings settings app
  where
    port = 8080
    settings = defaultSettings
      & setPort port
      & setBeforeMainLoop (hPutStrLn stderr $ "listening on port "<>show port)

    env = Env connPool

    corsPolicy = cors $ const $ Just $ simpleCorsResourcePolicy
      { corsOrigins = Just (["http://localhost:3000"], True)
      , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
      , corsRequestHeaders = ["Authorization", "Content-Type"]
      }
