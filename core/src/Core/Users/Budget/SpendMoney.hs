{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}

module Core.Users.Budget.SpendMoney (Dependencies, spendMoneyFromBudget, Error(..)) where

import Optics ((%), (^.), (^?))

import Data.Functor (void)

import SmartPrimitives.Positive (Positive)
import Core.Common.Domain.RubKopecks (RubKopecks)
import Core.Users.Domain.UserId (SomeUserId)
import Core.Users.MonadClasses.Repository
  (UsersRepository (getSomeUserFromRepo, trySetUserBudgetAmountInRepo))
import Core.Users.Budget.Domain.Budget (spendMoney, BudgetLowerBoundStatus)

data Error
  = UserDoesNotExist SomeUserId
  | UserDoesNotHaveBudget SomeUserId

type Dependencies m = UsersRepository m
spendMoneyFromBudget :: Dependencies m
                     => SomeUserId -> Positive RubKopecks
                     -> m (Either Error (RubKopecks, BudgetLowerBoundStatus))
spendMoneyFromBudget userId posKopecks =
  getSomeUserFromRepo userId >>= \case
    Nothing -> return $ Left $ UserDoesNotExist userId
    Just user -> do
      case spendMoney posKopecks <$> user ^? #data % #mBudget of
        Nothing -> return $ Left $ UserDoesNotHaveBudget userId
        Just (budget, lowerBoundStatus) -> do
          let budgetAmount = budget ^. #amount
          void $ trySetUserBudgetAmountInRepo userId budgetAmount
          return $ Right (budgetAmount, lowerBoundStatus)

