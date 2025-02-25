{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Core.Users.Budget.Create (Data(..), create) where

import Data.Maybe (fromMaybe)

import Core.Users.Budget.Domain.Budget (Budget(..))

data Data = Data
  { mInitialAmount :: Maybe Integer
  , mLowerBound :: Maybe Integer
  }

create :: Data -> Budget
create Data{ mInitialAmount, mLowerBound } =
  let amount = fromMaybe 0 mInitialAmount
   in Budget{..}
