{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module WebAPI.Users.Budget (BudgetAPI, budgetServer) where

import Servant (ServerT)
import Data.UUID (UUID)

import WebAPI.Users.Budget.ApplyDelta (ApplyBudgetDeltaToUser, applyBudgetDeltaToUser)
import qualified WebAPI.Users.Budget.ApplyDelta as ApplyDelta

type BudgetAPI = ApplyBudgetDeltaToUser

type Dependencies m = (ApplyDelta.Dependencies m)
budgetServer :: Dependencies m => UUID -> ServerT BudgetAPI m
budgetServer = applyBudgetDeltaToUser
