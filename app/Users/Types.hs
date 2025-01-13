{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Users.Types (User(..), UserId(..)) where

import Data.Text (Text)
import Data.UUID (UUID)
import Data.Aeson (ToJSON, FromJSON)
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.ToField (ToField)

import GHC.Generics (Generic)

import Groups.Types (GroupId)
import Servant (FromHttpApiData)

data User = User
  { username :: Text
  , groupId :: Maybe GroupId
  } deriving (Generic, FromRow, ToJSON)

newtype UserId = UserId UUID
  deriving (FromHttpApiData, ToField, FromJSON, ToJSON) via UUID
