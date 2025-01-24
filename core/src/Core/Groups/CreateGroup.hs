{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}

module Core.Groups.CreateGroup (CreateGroupData, Dependencies, createGroup) where

import Core.Groups.MonadClasses.Repository (GroupsRepository (addGroupToRepo))
import Core.Users.Domain.UserId (UserId)
import Core.Groups.Domain.GroupId (GroupId)
import SmartPrimitives.TextLenRange (TextLenRange)
import Core.Groups.Domain.Primitives (GroupName(GroupName))
import Core.Groups.Domain.Group (mkGroup)

data CreateGroupData = CreateGroupData
  { name :: TextLenRange 2 50
  , initialBudget :: Integer
  , members :: [UserId]
  , budgetLowerBound :: Maybe Integer
  }

type Dependencies m = (GroupsRepository m) -- UsersRepository to check if all the members exist
createGroup :: Dependencies m => CreateGroupData -> m GroupId
createGroup CreateGroupData{ name, initialBudget, members, budgetLowerBound } = do
  let groupName = GroupName name
  let group = mkGroup groupName initialBudget members budgetLowerBound
  addGroupToRepo group
