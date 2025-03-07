{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module WebAPI.Groups.GetAll
  ( Dependencies
  , getAllGroups
  ) where

import Servant (ServerT)

import Core.Users.Domain.UserId (UserId(UserId))
import qualified Core.Users.GetAllGroups as Impl (getAllGroups, Dependencies)
import CheckCheck.Contracts.Groups (GetAllGroups)
import CheckCheck.Contracts.Users (AuthenticatedUser(..))
import WebAPI.Groups.Get (toResp)

type Dependencies m = (Impl.Dependencies m)
getAllGroups :: Dependencies m => AuthenticatedUser -> ServerT GetAllGroups m
getAllGroups AUser{ userId } = map toResp <$> Impl.getAllGroups (UserId userId)
