{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell
  , DataKinds, FlexibleInstances
  , MultiParamTypeClasses, TypeFamilies
  , UndecidableInstances, TypeOperators
  #-}
{-# LANGUAGE FlexibleContexts #-}

module Receipts.Persistence
  ( getReceiptItemsFromDb
  , addReceiptItemsToDb
  , DbReceiptItem
  , createReceiptItemsTable
  ) where

import Database.PostgreSQL.Simple (FromRow, Only (Only))
import Database.PostgreSQL.Simple.ToField (ToField)
import Data.Text (Text)
import Optics.Label (LabelOptic)
import Optics
  ( to, view, Is, A_Getter
  , makeFieldLabelsWith, noPrefixFieldLabels, generateUpdateableOptics
  , (.~)
  )

import Control.Monad (void)
import Data.Function ((&))
import GHC.Generics (Generic)

import Shared.Persistence (MonadConnPoolReader, sql, query, execute_, executeMany)
import Shared.TuppledFieldsOptics (tuppledFields4)

data DbReceiptItem = DbReceiptItem
  { name :: Text
  , price :: Integer
  , quantity :: Double
  } deriving (Generic, FromRow)

makeFieldLabelsWith (noPrefixFieldLabels & generateUpdateableOptics .~ False) ''DbReceiptItem

getReceiptItemsFromDb :: MonadConnPoolReader m
                      => String -> m [DbReceiptItem]
getReceiptItemsFromDb qr = query [sql|
  SELECT name, price, quantity FROM receipt_items WHERE qr = ?
|] (Only qr)

-- addReceiptItemsToDb :: MonadConnPoolReader m
--                     => String -> [ReceiptItem] -> m ()
--                     Is this a good way to decouple from domain types?
--                     Should I even do that?
addReceiptItemsToDb ::
  ( Is name A_Getter,     LabelOptic "name"     name     a a nameT     nameT,     ToField nameT
  , Is price A_Getter,    LabelOptic "price"    price    a a priceT    priceT,    ToField priceT
  , Is quantity A_Getter, LabelOptic "quantity" quantity a a quantityT quantityT, ToField quantityT
  , MonadConnPoolReader f
  ) => String -> [a] -> f ()
addReceiptItemsToDb qr receiptItems = void $ executeMany [sql|
  INSERT INTO receipt_items (name, price, quantity, qr) VALUES (?, ?, ?, ?)
|] (tuppled <$> receiptItems)
  where tuppled = view $ tuppledFields4 #name #price #quantity (to $ const qr)

createReceiptItemsTable :: MonadConnPoolReader m => m ()
createReceiptItemsTable = void $ execute_ [sql|
  CREATE TABLE IF NOT EXISTS receipt_items
  ( qr TEXT NOT NULL
  , name TEXT NOT NULL
  , price INTEGER NOT NULL
  , quantity REAL NOT NULL
  )
|]

