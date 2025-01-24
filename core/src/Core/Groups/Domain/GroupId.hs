module Core.Groups.Domain.GroupId (GroupId(..), newGroupId) where

import Data.UUID (UUID)

import Core.Common.MonadClasses.MonadUUID (MonadUUID (newUUID))

newtype GroupId = GroupId UUID
  deriving (Eq)

newGroupId :: MonadUUID m => m GroupId
newGroupId = GroupId <$> newUUID

