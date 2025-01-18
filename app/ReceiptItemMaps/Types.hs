{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module ReceiptItemMaps.Types
  ( GlobalReceiptItemMap (GlobalReceiptItemMap)
  , PrivateReceiptItemMap (PrivateReceiptItemMap)
  , ReceiptItemMap (..)
  ) where

import Data.Text (Text)

import GHC.Generics (Generic)

import Shared.Types.Positive (Positive)
import Goods.Types (GoodsId)
import Groups.Types (GroupId)
import Database.PostgreSQL.Simple (FromRow)

newtype GlobalReceiptItemMap = GlobalReceiptItemMap ReceiptItemMap
newtype PrivateReceiptItemMap = PrivateReceiptItemMap ReceiptItemMap

data ReceiptItemMap = ReceiptItemMap
  { groupId :: Maybe GroupId
  , name :: Text
  , goodsId :: GoodsId
  , quantity :: Positive Double
  } deriving (Generic, FromRow)
