{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module WebAPI.Users.Budget.ApplyDelta
  ( Dependencies
  , applyBudgetDeltaToUser
  ) where

import Servant (HasServer(ServerT), err404, ServerError, err400, errBody)
import Data.UUID (toLazyASCIIBytes)
import Control.Monad.Error.Class (MonadError(throwError))

import Core.Common.Domain.RubKopecks (RubKopecks (..))
import Core.Users.Domain.UserId (SomeUserId (..))
import qualified Core.Users.Budget.ApplyDelta as Impl
  (Dependencies, applyBudgetDeltaToUser, Error(..))
import WebAPI.Users.Budget.Get (toResp)
import CheckCheck.Contracts.Users.Budget (ApplyBudgetDeltaToUser)
import CheckCheck.Contracts.Users (AuthenticatedUser(..))

type Dependencies m = (Impl.Dependencies m, MonadError ServerError m)
applyBudgetDeltaToUser :: Dependencies m => AuthenticatedUser -> ServerT ApplyBudgetDeltaToUser m
applyBudgetDeltaToUser AUser{ userId } delta =
  Impl.applyBudgetDeltaToUser (SomeUserId userId) (RubKopecks delta) >>= \case
    Right budget ->
      return $ toResp budget
    Left (Impl.UserDoesNotExist(SomeUserId uuid)) ->
      throwError err404
        { errBody = toLazyASCIIBytes uuid }
    Left (Impl.UserDoesNotHaveBudget(SomeUserId uuid)) ->
      throwError err400
        { errBody = "User " <> toLazyASCIIBytes uuid <> " does not have a budget" }


