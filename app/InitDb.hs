{-# LANGUAGE QuasiQuotes #-}

module InitDb (initDb) where

import Database.PostgreSQL.Simple.SqlQQ (sql)

import Control.Monad (void)

import Shared.Persistence (withTransaction, MonadConnPoolReader, execute_)
import Receipts.Persistence (createReceiptItemsTable)
import Users.Persistence (createUsersTable)
import Goods.Persistence (createGoodsTable)
import Groups.Persistence (createGroupsTable, createGroupsBudgetLogsTable, createGoodsStorageEntiresTable)
import ReceiptItemMaps.Persistence (createReceiptItemMapsTable, createReceiptItemMapsModerationEntriesTable)

initDb :: MonadConnPoolReader m => m ()
initDb = withTransaction $ do
  initExtensions
  createReceiptItemsTable
  createGoodsTable
  createGroupsTable
  createUsersTable
  createGroupsBudgetLogsTable
  createGoodsStorageEntiresTable
  createReceiptItemMapsTable
  createReceiptItemMapsModerationEntriesTable

initExtensions :: MonadConnPoolReader m => m ()
initExtensions = void $ execute_ [sql|
  CREATE EXTENSION IF NOT EXISTS "uuid-ossp"
|]
