{-# LANGUAGE DeriveGeneric #-}

module Groups.Types (Group(Group, name, budget), GroupId(GroupId)) where

import Data.Text (Text)
import Data.UUID (UUID)

import GHC.Generics (Generic)

data Group = Group
  { name :: Text
  , budget :: Integer
  } deriving Generic

newtype GroupId = GroupId UUID
  deriving Generic
