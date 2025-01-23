{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.ByteString.Char8 as B (pack)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Control.Monad.Reader (ReaderT (runReaderT))

import Control.Monad.IO.Class (MonadIO)
import Control.Monad (void)
import System.Environment (getEnv)

import Infrastructure.Common.Persistence (MonadConnReader, withTransaction, execute_)
import Infrastructure.Receipts.PGRepository (createReceiptItemsTable)

main :: IO ()
main =
  getEnv "POSTGRESQL_CONNECTION_STRING"
  >>= connectPostgreSQL . B.pack
  >>= runReaderT initDb

initDb :: (MonadIO m, MonadConnReader m) => m ()
initDb = withTransaction $ do
  initExtensions
  createReceiptItemsTable

initExtensions :: (MonadIO m, MonadConnReader m) => m ()
initExtensions = void $ execute_ [sql|
  CREATE EXTENSION IF NOT EXISTS "uuid-ossp"
|]
