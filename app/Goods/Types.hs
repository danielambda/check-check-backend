{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Goods.Types (Goods(..), GoodsId, GoodsUnits(..)) where

import Servant (FromHttpApiData)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple.ToField (ToField (toField), Action (Escape))
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Database.PostgreSQL.Simple (FromRow)

import GHC.Generics (Generic)
import Data.String (fromString)

import Shared.ByteStringParsableEnum (ByteStringParsableEnum)
import Shared.Persistence (mkEnumFieldParser)

data Goods = Goods
  { name :: Text
  , units :: GoodsUnits
  } deriving (Generic, FromRow, ToJSON)

newtype GoodsId = GoodsId UUID
  deriving newtype (FromHttpApiData, FromJSON, FromField, ToField, ToJSON)

data GoodsUnits
  = Grams
  | Liters
  | Pieces
  deriving (Show, Generic, ByteStringParsableEnum, FromJSON, ToJSON)

instance FromField GoodsUnits where
  fromField = mkEnumFieldParser "goods_units"

instance ToField GoodsUnits where
  toField = Escape . fromString . show
