{-# LANGUAGE DataKinds #-}

module Core.Users.MonadClasses.Repository
  ( UsersRepository(..)
  ) where

import Core.Common.Domain.RubKopecks (RubKopecks)
import Core.Users.Domain.User (User)
import Core.Users.Domain.UserId (UserId, SomeUserId)
import Core.Users.Domain.UserType (UserType(..))

class Monad m => UsersRepository m where
  addUserToRepo :: User t -> m ()
  getUserSingleFromRepo :: UserId 'Single -> m (Maybe (User 'Single))
  getUserGroupFromRepo :: UserId 'Group -> m (Maybe (User 'Group))
  userExistsInRepo :: SomeUserId -> m Bool
  tryApplyBudgetDeltaToUserInRepo :: SomeUserId -> RubKopecks -> m (Maybe RubKopecks)

