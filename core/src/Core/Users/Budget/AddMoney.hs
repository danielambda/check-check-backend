{-# LANGUAGE ConstraintKinds #-}

module Core.Users.Budget.AddMoney (Dependencies, addMoneyToBudget, Error(..)) where

import SmartPrimitives.Positive (Positive, unPositive)
import Core.Common.Domain.RubKopecks (RubKopecks)
import Core.Users.Domain.UserId (SomeUserId)
import Core.Users.MonadClasses.Repository (UsersRepository (userExistsInRepo, tryApplyBudgetDeltaToUserInRepo))
import Data.Functor ((<&>))

data Error
  = UserDoesNotExist SomeUserId
  | UserDoesNotHaveBudget SomeUserId

type Dependencies m = UsersRepository m
addMoneyToBudget :: Dependencies m => SomeUserId -> Positive RubKopecks -> m (Either Error RubKopecks )
addMoneyToBudget userId posKopecks = do
  userExists <- userExistsInRepo userId
  if not userExists then
    return $ Left $ UserDoesNotExist userId
  else
    let kopecks = unPositive posKopecks
    in tryApplyBudgetDeltaToUserInRepo userId kopecks <&>
      maybe (Left $ UserDoesNotHaveBudget userId) Right

