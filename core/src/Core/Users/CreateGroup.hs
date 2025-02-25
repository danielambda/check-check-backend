{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}

module Core.Users.CreateGroup
  ( Data(..), CreateBudgetData(..)
  , Dependencies
  , createGroup
  ) where

import Data.Maybe (catMaybes)
import Data.List.NonEmpty (NonEmpty, nonEmpty)

import SmartPrimitives.TextLenRange (TextLenRange)
import Core.Common.MonadClasses.MonadUUID (MonadUUID)
import Core.Common.Operators ((*>>))
import Core.Users.Common.CreateBudgetData (mkBudget, CreateBudgetData (CreateBudgetData))
import Core.Users.Domain.Primitives (Username(..))
import Core.Users.Domain.UserId (UserId, SomeUserId (SomeUserId))
import Core.Users.Domain.UserType (UserType(..))
import Core.Users.Domain.User (newUserGroup, User, UserData (..))
import Core.Users.MonadClasses.Repository (UsersRepository (addUserToRepo, userExistsInRepo))

data Data = CreateGroupData
  { name :: TextLenRange 2 50
  , ownerId :: UserId 'Single
  , otherUserIds :: [UserId 'Single]
  , mBudgetData :: Maybe CreateBudgetData
  }

newtype Error = NonExistingGroupMembers (NonEmpty (UserId 'Single))

type Dependencies m = (UsersRepository m, MonadUUID m)
createGroup :: Dependencies m => Data -> m (Either Error (User 'Group))
createGroup CreateGroupData{ name, ownerId, otherUserIds, mBudgetData } = do
  mNonExistingUserIds <- catMaybes <$> traverse userIdOfNonExisting (ownerId:otherUserIds)
  case nonEmpty mNonExistingUserIds of
    Just userIds ->
      return $ Left $ NonExistingGroupMembers userIds
    Nothing ->
      newUserGroup ownerId otherUserIds userData >>=
        (addUserToRepo *>> return . Right)
      where
        userData = UserData
          { username = Username name
          , mBudget = mkBudget <$> mBudgetData
          }

userIdOfNonExisting :: UsersRepository m => UserId t -> m (Maybe (UserId t))
userIdOfNonExisting userId = do
  userExists <- userExistsInRepo $ SomeUserId userId
  return $ if userExists then Nothing else Just userId
