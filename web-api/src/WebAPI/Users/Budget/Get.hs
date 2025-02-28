{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module WebAPI.Users.Budget.Get
  ( BudgetResp(..), toResp
  ) where

import Data.Aeson (ToJSON)
import Optics ((^.))

import GHC.Generics (Generic)

import Core.Common.Operators ((^^.), (^^?))
import Core.Users.Budget.Domain.Budget (Budget, BudgetLowerBoundStatus (BudgetLowerBoundExceeded))

data BudgetResp = BudgetResp
  { amount :: Integer
  , lowerBound :: Maybe Integer
  , isLowerBoundExceeded :: Bool
  } deriving (Generic, ToJSON)

toResp :: Budget -> BudgetResp
toResp budget = BudgetResp
  { amount = budget ^^. #amount
  , lowerBound = budget ^^? #mLowerBound
  , isLowerBoundExceeded = budget ^. #lowerBoundStatus == BudgetLowerBoundExceeded
  }

