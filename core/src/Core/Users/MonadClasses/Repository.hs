{-# LANGUAGE DataKinds #-}

module Core.Users.MonadClasses.Repository (UsersRepository(..)) where

import Data.Data (Typeable)

import Core.Users.Domain.User (User, SomeUser (SomeUser))
import Core.Users.Domain.UserId (UserId, SomeUserId)
import Core.Users.Domain.UserType (UserType(..))
import Core.Users.Domain.UserContact (UserContact)
import Data.Text (Text)
import Core.Users.Domain.Primitives (Username)

class Monad m => UsersRepository m where
  addUserToRepo :: User t -> m (Either Text ())
  getUserFromRepo :: Typeable t => UserId t -> m (Maybe (User t))
  getSomeUserFromRepo :: SomeUserId -> m (Maybe SomeUser)
  userExistsInRepo :: SomeUserId -> m Bool
  updateUserInRepo :: User t -> m ()
  updateUserInRepo = updateSomeUserInRepo . SomeUser
  updateSomeUserInRepo :: SomeUser -> m ()
  getGroupsOwnedByFromRepo :: UserId 'Single -> m [User 'Group]
  getGroupsParticipatedByFromRepo :: UserId 'Single -> m [User 'Group]
  getContactsWithUsernamesFromRepo :: UserId 'Single -> m [(UserContact, Username)]
  addContactToRepo :: UserId 'Single -> UserContact -> m ()
  deleteContactFromRepo :: UserId 'Single -> UserId 'Single -> m ()

