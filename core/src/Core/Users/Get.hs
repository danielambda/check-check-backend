{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

module Core.Users.Get (Dependencies, get) where

import Core.Users.Domain.UserId (UserId)
import Core.Users.Domain.UserType (UserType(Single))
import Core.Users.Domain.User (User)
import Core.Users.MonadClasses.Repository (UsersRepository(getUserSingleFromRepo))

type Dependencies m = (UsersRepository m)
get :: Dependencies m => UserId 'Single -> m (Maybe (User 'Single))
get = getUserSingleFromRepo
