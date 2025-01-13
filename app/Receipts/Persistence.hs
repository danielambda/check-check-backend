{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Receipts.Persistence
  ( getReceiptItemsFromDb
  , addReceiptItemsToDb
  , createReceiptItemsTable
  ) where

import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import Control.Monad (void)

import Receipts.Types (ReceiptItem (ReceiptItem, name, price, quantity))
import Common.Persistence (MonadConnPoolReader, query, executeMany, execute_)

getReceiptItemsFromDb :: MonadConnPoolReader m =>
                         String -> m [ReceiptItem]
getReceiptItemsFromDb qr = query [sql|
  SELECT name, price, quantity FROM receipt_items WHERE qr = ?
|] (Only qr)

addReceiptItemsToDb :: MonadConnPoolReader m =>
                       String -> [ReceiptItem] -> m ()
addReceiptItemsToDb qr receiptItems = void $ executeMany [sql|
  INSERT INTO receipt_items (qr, name, price, quantity) VALUES (?, ?, ?, ?)
|] (tuppled <$> receiptItems)
  where tuppled ReceiptItem{ name, price, quantity } = (qr, name, price, quantity)

createReceiptItemsTable :: MonadConnPoolReader m => m ()
createReceiptItemsTable = void $ execute_ [sql|
  CREATE TABLE IF NOT EXISTS receipt_items
  ( qr TEXT NOT NULL
  , name TEXT NOT NULL
  , price INTEGER NOT NULL
  , quantity REAL NOT NULL
  , PRIMARY KEY (qr, name)
  )
|]

