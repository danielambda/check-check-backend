{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Receipts.Types (Receipt(..), ReceiptItem(..)) where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import Database.PostgreSQL.Simple (FromRow)

newtype Receipt = Receipt
  { items :: [ReceiptItem]
  } deriving (Generic, ToJSON)

data ReceiptItem = ReceiptItem
  { name :: String
  , price :: Integer
  , quantity :: Double
  } deriving (Generic, FromRow, ToJSON, FromJSON)

