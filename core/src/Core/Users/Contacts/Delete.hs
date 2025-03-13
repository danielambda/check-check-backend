{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Core.Users.Contacts.Delete (Dependencies, deleteContact) where

import Core.Users.MonadClasses.Repository (UsersRepository(deleteContactFromRepo))
import Core.Users.Domain.UserId (UserId)
import Core.Users.Domain.UserType (UserType(Single))

type Dependencies m = UsersRepository m
deleteContact :: Dependencies m => UserId 'Single -> UserId 'Single -> m ()
deleteContact = deleteContactFromRepo
