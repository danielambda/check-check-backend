{-# LANGUAGE OverloadedLabels #-}

module WebAPI.Users.Budget.Get
  ( toResp
  ) where

import Optics ((^.))

import Core.Common.Operators ((^^.), (^^?))
import Core.Users.Budget.Domain.Budget (Budget, BudgetLowerBoundStatus (BudgetLowerBoundExceeded))
import CheckCheck.Contracts.Users.Budget (BudgetResp (..))

toResp :: Budget -> BudgetResp
toResp budget = BudgetResp
  { amount = budget ^^. #amount
  , lowerBound = budget ^^? #mLowerBound
  , isLowerBoundExceeded = budget ^. #lowerBoundStatus == BudgetLowerBoundExceeded
  }

