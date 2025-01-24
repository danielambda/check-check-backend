module Core.Groups.MonadClasses.Repository
  ( GroupsCommandRepository(..)
  , GroupsQueryRepository(..)
  ) where

import Data.Text (Text)

import SmartPrimitives.Positive (Positive)
import SmartPrimitives.NonNegative (NonNegative)
import Core.Groups.Domain.GroupId (GroupId)
import Core.Groups.Domain.Group (Group)
import Core.Groups.Domain.Budget (Budget)

class Monad m => GroupsCommandRepository m where
  addGroupToRepo :: Group -> m ()
  addMoneyToGroupBudgetInRepo :: GroupId -> Positive Integer -> m ()
  addGoodsToGroupInRepo :: GroupId -> [(Text, Positive Double, NonNegative Integer)] -> m ()

class Monad m => GroupsQueryRepository m where
  getGroupFromRepo :: GroupId -> m (Maybe Group)
  getGroupBudgetFromRepo :: GroupId -> m (Maybe Budget)
