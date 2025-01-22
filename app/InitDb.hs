{-# LANGUAGE QuasiQuotes #-}

module InitDb (initDb) where

import Database.PostgreSQL.Simple.SqlQQ (sql)

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)

import Shared.Persistence (withTransaction, MonadConnPoolReader, execute_)
import Receipts.Infrastructure.Persistence (createReceiptItemsTable)

initDb :: (MonadIO m, MonadConnPoolReader m) => m ()
initDb = withTransaction $ do
  initExtensions
  createReceiptItemsTable

initExtensions :: (MonadIO m, MonadConnPoolReader m) => m ()
initExtensions = void $ execute_ [sql|
  CREATE EXTENSION IF NOT EXISTS "uuid-ossp"
|]
