{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Receipts.Persistence
  ( getReceiptItemsFromDb
  , addReceiptItemsToDb
  , createReceiptItemsTable
  , dbToDomain
  ) where

import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Optics ((^.))

import Control.Monad (void)

import Shared.Persistence (MonadConnPoolReader, query, execute_, executeMany)
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Receipts.Domain.ReceiptItem (ReceiptItem, mkReceiptItem)
import Shared.Types.Positive (mkPositive)

data DbReceiptItem = DbReceiptItem
  { name :: Text
  , price :: Integer
  , quantity :: Double
  } deriving (Generic, FromRow)

dbToDomain :: DbReceiptItem -> Maybe ReceiptItem
dbToDomain DbReceiptItem{ name, price, quantity } = do
  posPrice <- mkPositive price
  posQuantity <- mkPositive quantity
  return $ mkReceiptItem (name, posPrice, posQuantity)

getReceiptItemsFromDb :: MonadConnPoolReader m
                      => String -> m [DbReceiptItem]
getReceiptItemsFromDb qr = query [sql|
  SELECT name, price, quantity FROM receipt_items WHERE qr = ?
|] (Only qr)

addReceiptItemsToDb :: MonadConnPoolReader m
                    => String -> [ReceiptItem] -> m ()
addReceiptItemsToDb qr receiptItems = void $ executeMany [sql|
  INSERT INTO receipt_items (qr, name, price, quantity) VALUES (?, ?, ?, ?)
|] (tuppled <$> receiptItems)
  where tuppled (item :: ReceiptItem) = (qr, item ^. #name, item ^. #price, item ^. #quantity)

createReceiptItemsTable :: MonadConnPoolReader m => m ()
createReceiptItemsTable = void $ execute_ [sql|
  CREATE TABLE IF NOT EXISTS receipt_items
  ( qr TEXT NOT NULL
  , name TEXT NOT NULL
  , price INTEGER NOT NULL
  , quantity REAL NOT NULL
  )
|]

