{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}

module Core.Groups.CreateGroup (CreateGroupData, Dependencies, createGroup) where

import Optics ((^.))

import SmartPrimitives.TextLenRange (TextLenRange)
import SmartPrimitives.Positive (Positive)
import Core.Common.MonadClasses.MonadUUID (MonadUUID)
import Core.Users.Domain.UserId (UserId)
import Core.Groups.MonadClasses.Repository
  (GroupsCommandRepository (addGroupToRepo, addMoneyToGroupBudgetInRepo))
import Core.Groups.Domain.GroupId (GroupId)
import Core.Groups.Domain.Primitives (GroupName(GroupName))
import Core.Groups.Domain.Group (mkNewGroup)
import Control.Monad (forM_)

data CreateGroupData = CreateGroupData
  { name :: TextLenRange 2 50
  , mInitialBudget :: Maybe (Positive Integer)
  , members :: [UserId]
  , budgetLowerBound :: Maybe Integer
  }

type Dependencies m = (GroupsCommandRepository m, MonadUUID m) -- UsersRepository to check if all the members exist
createGroup :: Dependencies m => CreateGroupData -> m GroupId
createGroup CreateGroupData{ name, members, mInitialBudget, budgetLowerBound } = do
  group <- mkNewGroup (GroupName name) members budgetLowerBound
  let groupId = group ^. #groupId

  addGroupToRepo group
  forM_ mInitialBudget $
    addMoneyToGroupBudgetInRepo groupId

  return groupId
