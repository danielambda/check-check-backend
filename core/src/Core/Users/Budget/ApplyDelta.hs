{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}

module Core.Users.Budget.ApplyDelta (Dependencies, applyBudgetDeltaToUser, Error(..)) where

import Optics ((%), (^.), (^?))

import Data.Functor (void)

import Core.Common.Domain.RubKopecks (RubKopecks)
import Core.Users.Domain.UserId (SomeUserId)
import Core.Users.MonadClasses.Repository
  (UsersRepository (getSomeUserFromRepo, trySetUserBudgetAmountInRepo))
import Core.Users.Budget.Domain.Budget (applyDelta, BudgetLowerBoundStatus)

data Error
  = UserDoesNotExist SomeUserId
  | UserDoesNotHaveBudget SomeUserId

-- TODO replace return type with just Budget
type Dependencies m = UsersRepository m
applyBudgetDeltaToUser :: Dependencies m
                       => SomeUserId -> RubKopecks
                       -> m (Either Error (RubKopecks, BudgetLowerBoundStatus))
applyBudgetDeltaToUser userId kopecks =
  getSomeUserFromRepo userId >>= \case
    Nothing -> return $ Left $ UserDoesNotExist userId
    Just user -> do
      case applyDelta kopecks <$> user ^? #data % #mBudget of
        Nothing -> return $ Left $ UserDoesNotHaveBudget userId
        Just budget -> do
          let budgetAmount = budget ^. #amount
          let budgetLowerBoundStatus = budget ^. #lowerBoundStatus
          void $ trySetUserBudgetAmountInRepo userId budgetAmount
          return $ Right (budgetAmount, budgetLowerBoundStatus)

