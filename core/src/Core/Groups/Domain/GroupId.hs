module Core.Groups.Domain.GroupId (GroupId) where

import Data.UUID (UUID)

newtype GroupId = GroupId UUID
  deriving (Eq)

