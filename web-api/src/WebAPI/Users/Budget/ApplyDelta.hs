{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module WebAPI.Users.Budget.ApplyDelta
  ( Dependencies
  , ApplyBudgetDeltaToUser
  , applyBudgetDeltaToUser
  ) where

import Servant (ReqBody, JSON, (:>), HasServer(ServerT), err404, ServerError, err400, errBody, Put)
import Data.UUID (UUID, toLazyASCIIBytes)
import Data.Aeson (ToJSON)
import Control.Monad.Error.Class (MonadError(throwError))

import GHC.Generics (Generic)

import Core.Common.Domain.RubKopecks (RubKopecks (..))
import Core.Users.Domain.UserId (UserId(..), SomeUserId (..))
import Core.Users.Budget.Domain.Budget (BudgetLowerBoundStatus(BudgetLowerBoundExceeded))
import qualified Core.Users.Budget.ApplyDelta as Impl
  (Dependencies, applyBudgetDeltaToUser, Error(..))

type ApplyBudgetDeltaToUser =
  ReqBody '[JSON] Integer :> Put '[JSON] ApplyBudgetDeltaToUserResp

data ApplyBudgetDeltaToUserResp = ApplyBudgetDeltaToUserResp
  { budget :: Integer
  , lowerBoundExeeded :: Bool
  } deriving (Generic, ToJSON)

type Dependencies m = (Impl.Dependencies m, MonadError ServerError m)
applyBudgetDeltaToUser :: Dependencies m => UUID -> ServerT ApplyBudgetDeltaToUser m
applyBudgetDeltaToUser userId delta =
  Impl.applyBudgetDeltaToUser (SomeUserId $ UserId userId) (RubKopecks delta) >>= \case
    Right (RubKopecks kopecks, budgetLbStatus) ->
      return $ ApplyBudgetDeltaToUserResp kopecks (budgetLbStatus == BudgetLowerBoundExceeded)
    Left (Impl.UserDoesNotExist _) ->
      throwError err404
    Left (Impl.UserDoesNotHaveBudget (SomeUserId (UserId userId'))) ->
      throwError err400
        { errBody = "User " <> toLazyASCIIBytes userId' <> " does not have a budget" }


