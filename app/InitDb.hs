{-# LANGUAGE QuasiQuotes #-}

module InitDb (initDb) where

import Database.PostgreSQL.Simple (Connection, withTransaction, execute_)
import Database.PostgreSQL.Simple.SqlQQ (sql)

import Control.Monad (void)

import Receipts.Persistence (createReceiptItemsTable)
import Users.Persistence (createUsersTable)
import Goods.Persistence (createGoodsTable)
import Groups.Persistence (createGroupsTable, createGroupsBudgetLogsTable, createGoodsStorageEntiresTable)
import ReceiptItemAssociations.Persistence (createReceiptItemAssociationsTable, createReceiptItemAssociationsModerationEntriesTable)

initDb :: Connection -> IO ()
initDb conn = withTransaction conn $ do
  initExtensions conn
  createReceiptItemsTable conn
  createGoodsTable conn
  createGroupsTable conn
  createUsersTable conn
  createGroupsBudgetLogsTable conn
  createGoodsStorageEntiresTable conn
  createReceiptItemAssociationsTable conn
  createReceiptItemAssociationsModerationEntriesTable conn

initExtensions :: Connection -> IO ()
initExtensions conn = void $ execute_ conn [sql|
  CREATE EXTENSION IF NOT EXISTS "uuid-ossp"
|]
