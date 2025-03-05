{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module WebAPI.Users.Budget (BudgetAPI, budgetServer) where

import Servant (ServerT, (:<|>) ((:<|>)))

import WebAPI.Auth (AuthenticatedUser)
import WebAPI.Users.Budget.ApplyDelta (ApplyBudgetDeltaToUser, applyBudgetDeltaToUser)
import qualified WebAPI.Users.Budget.ApplyDelta as ApplyDelta (Dependencies)
import WebAPI.Users.Budget.Create (CreateBudget, createBudget)
import qualified WebAPI.Users.Budget.Create as Create (Dependencies)

type BudgetAPI
  =    ApplyBudgetDeltaToUser
  :<|> CreateBudget

type Dependencies m = (ApplyDelta.Dependencies m, Create.Dependencies m)
budgetServer :: Dependencies m => AuthenticatedUser -> ServerT BudgetAPI m
budgetServer auser
  =    applyBudgetDeltaToUser auser
  :<|> createBudget auser
