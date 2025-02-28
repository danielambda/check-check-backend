{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module WebAPI.Users.Budget.ApplyDelta
  ( Dependencies
  , ApplyBudgetDeltaToUser
  , applyBudgetDeltaToUser
  ) where

import Servant (ReqBody, JSON, (:>), HasServer(ServerT), err404, ServerError, err400, errBody, Put)
import Data.UUID (UUID, toLazyASCIIBytes)
import Control.Monad.Error.Class (MonadError(throwError))

import Core.Common.Domain.RubKopecks (RubKopecks (..))
import Core.Users.Domain.UserId (UserId(..), SomeUserId (..))
import qualified Core.Users.Budget.ApplyDelta as Impl
  (Dependencies, applyBudgetDeltaToUser, Error(..))
import WebAPI.Users.Budget.Get (BudgetResp, toResp)

type ApplyBudgetDeltaToUser =
  ReqBody '[JSON] Integer :> Put '[JSON] BudgetResp

type Dependencies m = (Impl.Dependencies m, MonadError ServerError m)
applyBudgetDeltaToUser :: Dependencies m => UUID -> ServerT ApplyBudgetDeltaToUser m
applyBudgetDeltaToUser userId delta =
  Impl.applyBudgetDeltaToUser (SomeUserId $ UserId userId) (RubKopecks delta) >>= \case
    Right budget ->
      return $ toResp budget
    Left (Impl.UserDoesNotExist(SomeUserId(UserId uuid))) ->
      throwError err404
        { errBody = toLazyASCIIBytes uuid }
    Left (Impl.UserDoesNotHaveBudget(SomeUserId(UserId uuid))) ->
      throwError err400
        { errBody = "User " <> toLazyASCIIBytes uuid <> " does not have a budget" }


