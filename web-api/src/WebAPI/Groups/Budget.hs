{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module WebAPI.Groups.Budget (BudgetAPI, budgetServer, Dependencies) where

import Servant (ServerT, (:<|>) ((:<|>)))

import WebAPI.Groups.Budget.ApplyDelta (applyBudgetDeltaToUser)
import qualified WebAPI.Groups.Budget.ApplyDelta as ApplyDelta (Dependencies)
import WebAPI.Groups.Budget.Create (createBudget)
import qualified WebAPI.Groups.Budget.Create as Create (Dependencies)
import CheckCheck.Contracts.Users.Budget (BudgetAPI)
import CheckCheck.Contracts.Users (AuthenticatedUser)
import Data.UUID (UUID)

type Dependencies m = (ApplyDelta.Dependencies m, Create.Dependencies m)
budgetServer :: Dependencies m => AuthenticatedUser -> UUID -> ServerT BudgetAPI m
budgetServer auser groupId
  =    applyBudgetDeltaToUser auser groupId
  :<|> createBudget auser groupId
