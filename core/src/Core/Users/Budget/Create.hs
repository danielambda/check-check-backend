{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Core.Users.Budget.Create (Data(..), create) where

import Data.Maybe (fromMaybe)

import Core.Users.Budget.Domain.Budget (Budget(..))
import Core.Common.Domain.RubKopecks (RubKopecks(RubKopecks))

data Data = Data
  { mInitialAmount :: Maybe Integer
  , mLowerBound :: Maybe Integer
  }

create :: Data -> Budget
create Data{ mInitialAmount, mLowerBound } = Budget
  { amount = RubKopecks $ fromMaybe 0 mInitialAmount
  , mLowerBound = RubKopecks <$> mLowerBound
  }
