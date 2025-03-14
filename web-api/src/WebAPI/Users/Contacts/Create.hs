{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module WebAPI.Users.Contacts.Create (Dependencies, createContact) where

import Servant (ServerT, NoContent (NoContent))

import CheckCheck.Contracts.Users (AuthenticatedUser (..))
import CheckCheck.Contracts.Users.Contacts (CreateContact, CreateContactReqBody (..))
import qualified Core.Users.Contacts.Create as Impl (Dependencies, Data(..), createContact)
import Core.Users.Domain.UserId (UserId(UserId))

type Dependencies m = (Impl.Dependencies m)
createContact :: Dependencies m => AuthenticatedUser -> ServerT CreateContact m
createContact AUser{ userId } CreateContactReqBody{ contactUserId, contactName } = do
  let data' = Impl.Data
        { contactUserId = UserId contactUserId
        , contactName
        }
  NoContent <$ Impl.createContact (UserId userId) data'
