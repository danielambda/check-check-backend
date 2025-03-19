{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

module Core.Users.Contacts.GetAll (Dependencies, getContacts) where

import Core.Users.MonadClasses.Repository (UsersRepository(getContactsWithUsernamesFromRepo))
import Core.Users.Domain.UserId (UserId)
import Core.Users.Domain.UserType (UserType(Single))
import Core.Users.Domain.UserContact (UserContact)
import Core.Users.Domain.Primitives (Username)

type Dependencies m = (UsersRepository m)
getContacts :: Dependencies m => UserId 'Single -> m [(UserContact, Username)]
getContacts = getContactsWithUsernamesFromRepo
