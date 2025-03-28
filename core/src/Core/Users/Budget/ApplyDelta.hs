{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}

module Core.Users.Budget.ApplyDelta (Dependencies, applyBudgetDeltaToUser, Error(..)) where

import Optics ((%), (^?), (.~), (&))

import Core.Common.Domain.RubKopecks (RubKopecks)
import Core.Users.Domain.UserId (SomeUserId)
import Core.Users.MonadClasses.Repository
  (UsersRepository (getSomeUserFromRepo, updateSomeUserInRepo))
import Core.Users.Budget.Domain.Budget (applyDelta, Budget)

data Error
  = UserDoesNotExist SomeUserId
  | UserDoesNotHaveBudget SomeUserId

type Dependencies m = UsersRepository m
applyBudgetDeltaToUser :: Dependencies m => SomeUserId -> RubKopecks -> m (Either Error Budget)
applyBudgetDeltaToUser userId delta =
  getSomeUserFromRepo userId >>= \case
    Nothing -> return $ Left $ UserDoesNotExist userId
    Just user -> case applyDelta delta <$> user ^? #data % #mBudget of
      Nothing -> return $ Left $ UserDoesNotHaveBudget userId
      Just budget -> do
        updateSomeUserInRepo $ user & #data % #mBudget .~ budget
        return $ Right budget

