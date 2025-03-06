{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE LambdaCase #-}

module Core.Users.Budget.Create (Data(..), Dependencies, create, Error(..)) where

import Optics ((&), (%), (^?), (%~))

import Data.Maybe (fromMaybe)

import Core.Users.Budget.Domain.Budget (Budget(..))
import Core.Common.Domain.RubKopecks (RubKopecks)
import Core.Users.MonadClasses.Repository (UsersRepository (getSomeUserFromRepo, updateSomeUserInRepo))
import Core.Users.Domain.UserId (SomeUserId)
import Core.Users.Domain.User (UserData(mBudget))

data Data = Data
  { mInitialAmount :: Maybe RubKopecks
  , mLowerBound :: Maybe RubKopecks
  }

data Error
  = UserDoesNotExist SomeUserId
  | UserAlreadyHasBudget SomeUserId

type Dependencies m = (UsersRepository m)
create :: Dependencies m => SomeUserId -> Data -> m (Either Error Budget)
create userId Data{ mInitialAmount, mLowerBound } = do
  getSomeUserFromRepo userId >>= \case
    Nothing -> return $ Left $ UserDoesNotExist userId
    Just user -> case user ^? #data % #mBudget of
      Just _ -> return $ Left $ UserAlreadyHasBudget userId
      Nothing -> do
        let budget = Budget
              { amount = fromMaybe 0 mInitialAmount
              , mLowerBound = mLowerBound
              }
        updateSomeUserInRepo $ user & #data %~ \d -> d{ mBudget = Just budget }
        return $ Right budget
