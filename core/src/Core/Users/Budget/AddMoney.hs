{-# LANGUAGE ConstraintKinds #-}

module Core.Users.Budget.AddMoney (Dependencies, addMoneyToBudget, Error(..)) where

import SmartPrimitives.Positive (Positive, unPositive)
import Core.Common.Domain.RubKopecks (RubKopecks)
import Core.Users.Domain.UserId (SomeUserId)
import Core.Users.MonadClasses.Repository (UsersRepository (userExistsInRepo, tryApplyBudgetDeltaToUserInRepo))

data Error
  = UserDoesNotExist SomeUserId
  | UserDoesNotHaveBudget SomeUserId

type Dependencies m = UsersRepository m
addMoneyToBudget :: Dependencies m => SomeUserId -> Positive RubKopecks -> m (Maybe Error)
addMoneyToBudget userId posKopecks = do
  userExists <- userExistsInRepo userId
  if not userExists then
    return $ Just $ UserDoesNotExist userId
  else do
    let kopecks = unPositive posKopecks
    hasAddedMoney <- tryApplyBudgetDeltaToUserInRepo userId kopecks
    if hasAddedMoney then
      return Nothing
    else
      return $ Just $ UserDoesNotHaveBudget userId

