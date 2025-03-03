{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module WebAPI.Users.Budget (BudgetAPI, budgetServer) where

import Servant (ServerT)

import WebAPI.Users.Budget.ApplyDelta (ApplyBudgetDeltaToUser, applyBudgetDeltaToUser)
import qualified WebAPI.Users.Budget.ApplyDelta as ApplyDelta
import WebAPI.Auth (AuthenticatedUser)

type BudgetAPI = ApplyBudgetDeltaToUser

type Dependencies m = (ApplyDelta.Dependencies m)
budgetServer :: Dependencies m => AuthenticatedUser -> ServerT BudgetAPI m
budgetServer = applyBudgetDeltaToUser
