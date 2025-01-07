{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Receipts.Persistence (getReceiptItemsFromDb, addReceiptItemsToDb, initReceiptItemsTable) where

import Database.PostgreSQL.Simple (Connection, query, Only (Only), execute_, executeMany)
import Database.PostgreSQL.Simple.SqlQQ (sql)

import Receipts.Types (ReceiptItem (ReceiptItem, name, price, quantity))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad (void)

getReceiptItemsFromDb :: MonadIO m => Connection -> String -> m [ReceiptItem]
getReceiptItemsFromDb conn qr = liftIO $ query conn
  "SELECT name, price, quantity FROM receiptItems WHERE qr = ?" (Only qr)

addReceiptItemsToDb :: MonadIO m => Connection -> String -> [ReceiptItem] -> m ()
addReceiptItemsToDb conn qr receiptItems = void $ liftIO $ executeMany conn [sql|
  INSERT INTO receiptItems (qr, name, price, quantity) VALUES (?, ?, ?, ?)
|] $ map tuppled receiptItems
  where tuppled ReceiptItem{ name, price, quantity } = (qr, name, price, quantity)

initReceiptItemsTable :: MonadIO m => Connection -> m ()
initReceiptItemsTable conn = void $ liftIO $ execute_ conn [sql|
  CREATE TABLE IF NOT EXISTS receiptItems
  ( qr TEXT NOT NULL
  , name TEXT NOT NULL
  , price INTEGER NOT NULL
  , quantity REAL NOT NULL
  )
|]
