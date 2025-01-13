{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Receipts.Types (Receipt(..), ReceiptItem(..)) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow)

import GHC.Generics

newtype Receipt = Receipt
  { items :: [ReceiptItem]
  } deriving (Generic, ToJSON)

data ReceiptItem = ReceiptItem
  { name :: Text
  , price :: Integer
  , quantity :: Double
  } deriving (Generic, FromRow, ToJSON, FromJSON)

