{-# LANGUAGE DeriveGeneric #-}

module ReceiptItemMaps.Types (ReceiptItemMap(..)) where

import Data.Text (Text)

import GHC.Generics (Generic)

import Goods.Types (GoodsId)

data ReceiptItemMap = ReceiptItemMap
  { name :: Text
  , goodsId :: GoodsId
  , isGlobal :: Bool
  } deriving (Generic)
