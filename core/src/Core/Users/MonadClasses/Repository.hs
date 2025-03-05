{-# LANGUAGE DataKinds #-}

module Core.Users.MonadClasses.Repository (UsersRepository(..)) where

import Data.Data (Typeable)

import Core.Common.Domain.RubKopecks (RubKopecks)
import Core.Users.Domain.User (User, SomeUser)
import Core.Users.Domain.UserId (UserId, SomeUserId)

class Monad m => UsersRepository m where
  addUserToRepo :: User t -> m ()
  getUserFromRepo :: Typeable t => UserId t -> m (Maybe (User t))
  getSomeUserFromRepo :: SomeUserId -> m (Maybe SomeUser)
  userExistsInRepo :: SomeUserId -> m Bool
  tryApplyBudgetDeltaToUserInRepo :: SomeUserId -> RubKopecks -> m (Maybe RubKopecks)
  trySetUserBudgetAmountInRepo :: SomeUserId -> RubKopecks -> m Bool

