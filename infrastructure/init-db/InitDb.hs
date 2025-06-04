{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.ByteString.Char8 as B (pack)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Control.Monad.Reader (ReaderT (runReaderT))

import Control.Monad (void)
import System.Environment (getEnv)

import Infrastructure.Common.Persistence (MonadPG, execute_)

main :: IO ()
main = getEnv "POSTGRESQL_CONNECTION_STRING"
  >>= connectPostgreSQL . B.pack
  >>= runReaderT initDb

initDb :: MonadPG m => m ()
initDb = void $ execute_ [sql|
  create table if not exists requests
  ( request_id UUID NOT NULL PRIMARY KEY
  , created_at TIMESTAMPTZ NOT NULL
  , is_pending BOOL NOT NULL
  );

  create table if not exists request_items
  ( request_id UUID NOT NULL REFERENCES requests(request_id)
  , name TEXT NOT NULL
  , price INTEGER NOT NULL
  )
|]

