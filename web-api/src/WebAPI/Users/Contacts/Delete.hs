{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module WebAPI.Users.Contacts.Delete (Dependencies, deleteContact) where

import Servant (ServerT, NoContent (NoContent))

import CheckCheck.Contracts.Users (AuthenticatedUser (..))
import CheckCheck.Contracts.Users.Contacts (DeleteContact)
import Core.Users.Domain.UserId (UserId(UserId))
import qualified Core.Users.Contacts.Delete as Impl (Dependencies, deleteContact)

type Dependencies m = (Impl.Dependencies m)
deleteContact :: Dependencies m => AuthenticatedUser -> ServerT DeleteContact m
deleteContact AUser{ userId } contactUserId = NoContent <$
  Impl.deleteContact (UserId userId) (UserId contactUserId)
