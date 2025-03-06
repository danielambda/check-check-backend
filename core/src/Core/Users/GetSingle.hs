{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

module Core.Users.GetSingle (Dependencies, getSingle) where

import Core.Users.Domain.UserId (UserId)
import Core.Users.Domain.UserType (UserType(Single))
import Core.Users.Domain.User (User)
import Core.Users.MonadClasses.Repository (UsersRepository(getUserFromRepo))

type Dependencies m = (UsersRepository m)
getSingle :: Dependencies m => UserId 'Single -> m (Maybe (User 'Single))
getSingle = getUserFromRepo
