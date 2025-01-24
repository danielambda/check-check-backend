module Core.Groups.MonadClasses.Repository
  ( GroupsCommandRepository(..)
  , GroupsQueryRepository(..)
  ) where

import Core.Groups.Domain.Group (Group)
import Core.Groups.Domain.GroupId (GroupId)
import SmartPrimitives.Positive (Positive)

class Monad m => GroupsCommandRepository m where
  addGroupToRepo :: Group -> m ()
  addMoneyToGroupBudgetInRepo :: GroupId -> Positive Integer -> m ()

class Monad m => GroupsQueryRepository m where
  getGroupFromRepo :: GroupId -> m (Maybe Group)
