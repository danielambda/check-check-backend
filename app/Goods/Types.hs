{-# LANGUAGE DeriveGeneric #-}

module Goods.Types (Goods(..), GoodsId, GoodsUnits(..)) where

import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data Goods = Goods
  { id :: GoodsId
  , name :: Text
  , units :: GoodsUnits
  } deriving Generic

newtype GoodsId = GoodsId UUID

data GoodsUnits
  = Grams
  | Liters
  | Pieces

