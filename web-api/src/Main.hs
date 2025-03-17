{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Network.Wai.Handler.Warp
  (defaultSettings, setPort, setBeforeMainLoop, runSettings, setHost)
import Network.Wai.Middleware.Cors
  (cors, simpleCorsResourcePolicy, corsMethods, corsOrigins, corsRequestHeaders)
import Data.ByteString.Char8 as BS8 (pack)
import Data.Pool (Pool, newPool, defaultPoolConfig, destroyAllResources)

import System.IO (hPutStrLn, stderr)
import System.Environment (getEnv)
import Control.Exception (finally)
import Data.Function ((&))

import WebAPI.AppM (Env(..))
import WebAPI (mkApp)

import qualified Database.PostgreSQL.Simple as PG (Connection, connectPostgreSQL, close)
import qualified Database.Redis as Redis

main :: IO ()
main = do
  pgConn <- PG.connectPostgreSQL . BS8.pack <$> getEnv "POSTGRESQL_CONNECTION_STRING"
  pgConnPool <- newPool $ defaultPoolConfig pgConn PG.close
                            20 -- Keepalive time (seconds)
                            50 -- Max resourcesction is kept
  -- redisConnPool <- newPool $ defaultPoolConfig undefined undefined
  --                     20 -- Keepalive time (seconds)
  --                     50 -- Max resourcesction is kept
  runAPI pgConnPool undefined
    `finally` do
      destroyAllResources pgConnPool
      -- destroyAllResources redisConnPool

runAPI :: Pool PG.Connection -> Pool Redis.Connection -> IO ()
runAPI pgConnPool redisConnPool = do
  let middlewares = corsMiddleware
  app <- middlewares <$> mkApp Env{..}
  runSettings settings app
  where
    port = 8080
    settings = defaultSettings
      & setHost "0.0.0.0"
      & setPort port
      & setBeforeMainLoop (hPutStrLn stderr $ "listening on port "<>show port)

    corsMiddleware = cors $ const $ Just $ simpleCorsResourcePolicy
      { corsOrigins = Just (["http://localhost:3000"], True)
      , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
      , corsRequestHeaders = ["Authorization", "Content-Type"]
      }
