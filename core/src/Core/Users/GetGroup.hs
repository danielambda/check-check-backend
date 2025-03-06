{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

module Core.Users.GetGroup (Dependencies, getGroup) where

import Core.Users.Domain.UserId (UserId)
import Core.Users.Domain.UserType (UserType(Group))
import Core.Users.Domain.User (User)
import Core.Users.MonadClasses.Repository (UsersRepository(getUserFromRepo))

type Dependencies m = (UsersRepository m)
getGroup :: Dependencies m => UserId 'Group -> m (Maybe (User 'Group))
getGroup = getUserFromRepo
