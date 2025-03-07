{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module WebAPI.Groups.Create
  ( Dependencies
  , createGroup
  ) where

import Servant (ServerT, ServerError, err400)

import Control.Monad.Error.Class (throwError, MonadError)

import Core.Users.Domain.UserId (UserId(UserId))
import qualified Core.Users.CreateGroup as Impl (Data(..), createGroup, Dependencies, Error(..))
import WebAPI.Groups.Get (toResp)
import CheckCheck.Contracts.Groups (CreateGroupReqBody(..), CreateGroup)
import CheckCheck.Contracts.Users (AuthenticatedUser(..))
import WebAPI.Auth (ensureUserExistsInRepo)

type Dependencies m = (Impl.Dependencies m, MonadError ServerError m)
createGroup :: Dependencies m => AuthenticatedUser -> ServerT CreateGroup m
createGroup auser@AUser{ userId } CreateGroupReqBody{..} = do
  ensureUserExistsInRepo auser
  let data' = Impl.Data
        { Impl.name = name
        , Impl.ownerId = UserId userId
        , Impl.otherUserIds = UserId <$> concat otherUserIds
        }
  Impl.createGroup data' >>= \case
    Right group -> return $ toResp group
    Left (Impl.NonExistingGroupMembers _) -> throwError err400

