{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module WebAPI.Users.Budget (Dependencies, budgetServer) where

import Servant (ServerT, (:<|>) ((:<|>)))

import WebAPI.Users.Budget.ApplyDelta (applyBudgetDeltaToUser)
import qualified WebAPI.Users.Budget.ApplyDelta as ApplyDelta (Dependencies)
import WebAPI.Users.Budget.Create (createBudget)
import qualified WebAPI.Users.Budget.Create as Create (Dependencies)
import CheckCheck.Contracts.Users.Budget (BudgetAPI)
import CheckCheck.Contracts.Users (AuthenticatedUser)

type Dependencies m = (ApplyDelta.Dependencies m, Create.Dependencies m)
budgetServer :: Dependencies m => AuthenticatedUser -> ServerT BudgetAPI m
budgetServer auser
  =    applyBudgetDeltaToUser auser
  :<|> createBudget auser
