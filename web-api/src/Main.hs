{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Network.Wai.Handler.Warp
  (defaultSettings, setPort, setBeforeMainLoop, runSettings, setHost)
import Data.ByteString.Char8 as BS8 (pack)
import Data.Pool (Pool, newPool, defaultPoolConfig, destroyAllResources)

import System.IO (hPutStrLn, stderr)
import System.Environment (getEnv)
import Control.Concurrent (newMVar)
import Control.Exception (finally)
import Data.Function ((&))

import WebAPI.AppM (Env(..))
import WebAPI (mkApp)

import qualified Database.PostgreSQL.Simple as PG (Connection, connectPostgreSQL, close)

main :: IO ()
main = do
  pgConn <- PG.connectPostgreSQL . BS8.pack <$> getEnv "POSTGRESQL_CONNECTION_STRING"
  pgConnPool <- newPool $ defaultPoolConfig pgConn PG.close
                            20 -- Keepalive time (seconds)
                            50 -- Max resourcesction is kept
  runAPI pgConnPool
    `finally` do
      destroyAllResources pgConnPool

runAPI :: Pool PG.Connection -> IO ()
runAPI pgConnPool = do
  budgetAmountMVar <- newMVar 0
  app <- mkApp Env{..}
  runSettings settings app
  where
    port = 8080
    settings = defaultSettings
      & setHost "0.0.0.0"
      & setPort port
      & setBeforeMainLoop (hPutStrLn stderr $ "listening on port "<>show port)
