{-# LANGUAGE DeriveGeneric #-}

module Receipts.Types (Receipt(..), ReceiptItem(..)) where

import GHC.Generics

newtype Receipt = Receipt
  { items :: [ReceiptItem]
  } deriving Generic

data ReceiptItem = ReceiptItem
  { name :: String
  , price :: Integer
  , quantity :: Double
  } deriving Generic

