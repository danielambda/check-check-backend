{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Receipts.Persistence
  ( getReceiptItemsFromDb
  , addReceiptItemsToDb
  , initReceiptItemsTable
  ) where

import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import Control.Monad.IO.Class (MonadIO)
import Control.Monad (void)

import Receipts.Types (ReceiptItem (ReceiptItem, name, price, quantity))
import Common.Persistence (MonadConnPoolReader, query, execute_, executeMany)

getReceiptItemsFromDb :: (MonadIO m, MonadConnPoolReader m) =>
                         String -> m [ReceiptItem]
getReceiptItemsFromDb qr = query [sql|
  SELECT name, price, quantity FROM receiptItems WHERE qr = ?
|] (Only qr)

addReceiptItemsToDb :: (MonadIO m, MonadConnPoolReader m) =>
                       String -> [ReceiptItem] -> m ()
addReceiptItemsToDb qr receiptItems = void $ executeMany [sql|
  INSERT INTO receiptItems (qr, name, price, quantity) VALUES (?, ?, ?, ?)
|] (map tuppled receiptItems)
  where tuppled ReceiptItem{ name, price, quantity } = (qr, name, price, quantity)

initReceiptItemsTable :: (MonadIO m, MonadConnPoolReader m) => m ()
initReceiptItemsTable = void $ execute_ [sql|
  CREATE TABLE IF NOT EXISTS receiptItems
  ( qr TEXT NOT NULL
  , name TEXT NOT NULL
  , price INTEGER NOT NULL
  , quantity REAL NOT NULL
  , PRIMARY KEY (qr, name)
  )
|]
