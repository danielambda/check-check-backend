{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

module Core.Users.GetContacts (Dependencies, getContacts) where

import Core.Users.MonadClasses.Repository (UsersRepository(getContactsFromRepo))
import Core.Users.Domain.UserId (UserId)
import Core.Users.Domain.UserType (UserType(Single))
import Core.Users.Domain.UserContact (UserContact)

type Dependencies m = (UsersRepository m)
getContacts :: Dependencies m => UserId 'Single -> m [UserContact]
getContacts = getContactsFromRepo
