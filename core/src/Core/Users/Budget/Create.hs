{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE LambdaCase #-}

module Core.Users.Budget.Create (Data(..), Dependencies, create, Error(..)) where

import Optics ((&), (%), (.~))

import Data.Maybe (fromMaybe)

import Core.Users.Budget.Domain.Budget (Budget(..))
import Core.Common.Domain.RubKopecks (RubKopecks(RubKopecks))
import Core.Users.MonadClasses.Repository (UsersRepository (getSomeUserFromRepo, updateSomeUserInRepo))
import Core.Users.Domain.UserId (SomeUserId)

data Data = Data
  { mInitialAmount :: Maybe RubKopecks
  , mLowerBound :: Maybe RubKopecks
  }

newtype Error = UserDoesNotExist SomeUserId

type Dependencies m = (UsersRepository m)
create :: Dependencies m => SomeUserId -> Data -> m (Either Error Budget)
create userId Data{ mInitialAmount, mLowerBound } = do
  getSomeUserFromRepo userId >>= \case
    Nothing -> return $ Left $ UserDoesNotExist userId
    Just user -> do
      let budget = Budget
            { amount = fromMaybe 0 mInitialAmount
            , mLowerBound = mLowerBound
            }
      updateSomeUserInRepo $ user & #data % #mBudget .~ budget
      return $ Right budget
