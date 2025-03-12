{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Core.Users.Contacts.Create (Dependencies, Data(..), createContact) where

import Data.Function ((&))

import SmartPrimitives.TextMaxLen (TextMaxLen)
import Core.Common.Operators ((*>>))
import Core.Users.MonadClasses.Repository (UsersRepository(addContactToRepo))
import Core.Users.Domain.UserId (UserId)
import Core.Users.Domain.UserType (UserType(Single))
import Core.Users.Domain.UserContact (UserContact (..))

data Data = Data
  { contactUserId :: UserId 'Single
  , contactName :: Maybe (TextMaxLen 50)
  }

type Dependencies m = UsersRepository m
createContact :: Dependencies m => UserId 'Single -> Data -> m UserContact
createContact userId Data{ contactUserId, contactName = mContactName } =
  UserContact{..} & addContactToRepo userId *>> return
