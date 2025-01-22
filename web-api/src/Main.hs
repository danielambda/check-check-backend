{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Configuration.Dotenv (loadFile, defaultConfig)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
  (cors, simpleCorsResourcePolicy, corsMethods, corsOrigins, corsRequestHeaders)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import Data.ByteString.Char8 as B (pack)

import System.Environment (getEnv)

import WebAPI (Env(..), application)

main :: IO ()
main = do
  loadFile defaultConfig

  getEnv "POSTGRESQL_CONNECTION_STRING"
  >>= connectPostgreSQL . B.pack
  >>= runAPI

runAPI :: Connection -> IO ()
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
