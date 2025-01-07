{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Receipts.Types
  ( Receipt(Receipt)
  , ReceiptItem(ReceiptItem, name, price, quantity)
  ) where

import Data.Aeson

import GHC.Generics
import Database.PostgreSQL.Simple (FromRow)

newtype Receipt = Receipt
  { items :: [ReceiptItem]
  } deriving stock (Generic)
    deriving anyclass (ToJSON)

data ReceiptItem = ReceiptItem
  { name :: String
  , price :: Integer
  , quantity :: Double
  } deriving stock (Generic)
    deriving anyclass (ToJSON, FromJSON, FromRow)

