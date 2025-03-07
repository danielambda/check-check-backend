{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}

module Core.Users.CreateGroup
  ( Data(..)
  , Dependencies
  , createGroup
  , Error(..)
  ) where

import Data.List.NonEmpty (NonEmpty, nonEmpty)

import SmartPrimitives.TextLenRange (TextLenRange)
import Core.Common.MonadClasses.MonadUUID (MonadUUID)
import Core.Common.Operators ((*>>))
import Core.Users.MonadClasses.Repository (UsersRepository (addUserToRepo, userExistsInRepo))
import Core.Users.Domain.Primitives (Username(..))
import Core.Users.Domain.UserId (UserId (..), someUserId)
import Core.Users.Domain.UserType (UserType(..))
import Core.Users.Domain.User (newUserGroup, User, UserData (..))
import Control.Monad (filterM)

data Data = Data
  { name :: TextLenRange 2 50
  , ownerId :: UserId 'Single
  , otherUserIds :: [UserId 'Single]
  }

newtype Error = NonExistingGroupMembers (NonEmpty (UserId 'Single))

type Dependencies m = (UsersRepository m, MonadUUID m)
createGroup :: Dependencies m => Data -> m (Either Error (User 'Group))
createGroup Data{ name, ownerId, otherUserIds } = do
  mNonExistingUserIds <- filterM (fmap not . userExistsInRepo . someUserId) (ownerId:otherUserIds)
  case nonEmpty mNonExistingUserIds of
    Just userIds ->
      return $ Left $ NonExistingGroupMembers userIds
    Nothing ->
      newUserGroup ownerId otherUserIds userData >>=
        (addUserToRepo *>> return . Right)
      where
        userData = UserData
          { username = Username name
          , mBudget = Nothing
          }
