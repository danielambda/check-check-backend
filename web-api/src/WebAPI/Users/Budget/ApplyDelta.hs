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

import SmartPrimitives.Utils (posZeroNeg, PosZeroNeg(..))
import Core.Common.Domain.RubKopecks (positiveRubKopecks, RubKopecks (..))
import Core.Users.Domain.UserId (UserId(..), SomeUserId (..))
import qualified Core.Users.Budget.AddMoney as AddImpl (Dependencies, addMoneyToBudget, Error (..))

type ApplyBudgetDeltaToUser =
  ReqBody '[JSON] Integer :> Put '[JSON] ApplyBudgetDeltaToUserResp

data ApplyBudgetDeltaToUserResp = ApplyBudgetDeltaToUserResp
  { budget :: Integer
  , lowerBoundExeeded :: Bool
  } deriving (Generic, ToJSON)

type Dependencies m = (AddImpl.Dependencies m, MonadError ServerError m)
applyBudgetDeltaToUser :: Dependencies m => UUID -> ServerT ApplyBudgetDeltaToUser m
applyBudgetDeltaToUser userId delta = case posZeroNeg delta of
  Pos posDelta -> do
    AddImpl.addMoneyToBudget (SomeUserId $ UserId userId) (positiveRubKopecks posDelta) >>= \case
      Right (RubKopecks kopecks) ->
        return $ ApplyBudgetDeltaToUserResp kopecks False
      Left (AddImpl.UserDoesNotExist _) ->
        throwError err404
      Left (AddImpl.UserDoesNotHaveBudget (SomeUserId (UserId userId'))) ->
        throwError err400
          { errBody = "User " <> toLazyASCIIBytes userId' <> " does not have a budget" }
  Zero ->
    throwError err400 { errBody = "Zero delta is invalid" }
  Neg _negDelta -> do
    throwError err400 { errBody = "Negative delta is not implemented yet" }
