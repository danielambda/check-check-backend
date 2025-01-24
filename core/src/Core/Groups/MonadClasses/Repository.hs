module Core.Groups.MonadClasses.Repository (GroupsRepository(..)) where

import Core.Groups.Domain.Group (Group)
import Core.Groups.Domain.GroupId (GroupId)

class Monad m => GroupsRepository m where
  addGroupToRepo :: Group -> m GroupId
  getGroupFromRepo :: GroupId -> m (Maybe Group)
