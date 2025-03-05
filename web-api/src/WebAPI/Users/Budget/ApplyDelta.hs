{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module WebAPI.Users.Budget.ApplyDelta
  ( Dependencies
  , ApplyBudgetDeltaToUser
  , applyBudgetDeltaToUser
  ) where

import Servant (ReqBody, JSON, (:>), HasServer(ServerT), err404, ServerError, err400, errBody, Patch)
import Data.UUID (toLazyASCIIBytes)
import Control.Monad.Error.Class (MonadError(throwError))

import Core.Common.Domain.RubKopecks (RubKopecks (..))
import Core.Users.Domain.UserId (SomeUserId (..))
import qualified Core.Users.Budget.ApplyDelta as Impl
  (Dependencies, applyBudgetDeltaToUser, Error(..))
import WebAPI.Auth (AuthenticatedUser(AUser, userId))
import WebAPI.Users.Budget.Get (BudgetResp, toResp)

type ApplyBudgetDeltaToUser =
  ReqBody '[JSON] Integer :> Patch '[JSON] BudgetResp

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


