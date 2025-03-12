{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module WebAPI.Users.Contacts.GetAll (Dependencies, getContacts, toResp) where

import Servant (ServerT)
import Optics ((<&>), (&), (^.))

import CheckCheck.Contracts.Users.Contacts (GetContacts, ContactResp(..))
import CheckCheck.Contracts.Users (AuthenticatedUser (..))
import Core.Common.Operators ((^^.))
import Core.Users.Domain.UserId (UserId(UserId))
import Core.Users.Domain.UserContact (UserContact)
import qualified Core.Users.Contacts.GetAll as Impl (getContacts, Dependencies)

type Dependencies m = (Impl.Dependencies m)
getContacts :: Dependencies m => AuthenticatedUser -> ServerT GetContacts m
getContacts AUser{ userId } = userId
   &  UserId
   &  Impl.getContacts
  <&> map toResp

toResp :: UserContact -> ContactResp
toResp contact = ContactResp
  { contactUserId = contact ^^. #contactUserId
  , contactName = contact ^. #mContactName
  }
