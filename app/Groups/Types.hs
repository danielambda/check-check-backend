{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Groups.Types (Group(Group, name, budget), GroupId(GroupId)) where

import Data.Text (Text)
import Data.UUID (UUID)
import Data.Aeson (ToJSON, FromJSON)
import Servant (FromHttpApiData)
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)

import GHC.Generics (Generic)

data Group = Group
  { name :: Text
  , budget :: Integer
  } deriving (Generic, FromRow, ToJSON)

newtype GroupId = GroupId UUID
  deriving newtype (FromHttpApiData, FromField, ToField, FromJSON, ToJSON)

