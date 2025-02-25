{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Core.Users.Common.CreateBudgetData
  ( CreateBudgetData(..)
  , mkBudget
  ) where

import Data.Maybe (fromMaybe)

import Core.Users.Domain.Budget (Budget(..))

data CreateBudgetData = CreateBudgetData
  { mInitialAmount :: Maybe Integer
  , mLowerBound :: Maybe Integer
  }

mkBudget :: CreateBudgetData -> Budget
mkBudget CreateBudgetData{ mInitialAmount, mLowerBound } =
  let amount = fromMaybe 0 mInitialAmount
   in Budget{..}
