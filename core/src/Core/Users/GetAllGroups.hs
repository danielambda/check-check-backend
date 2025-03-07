{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

module Core.Users.GetAllGroups (Dependencies, getAllGroups) where

import Core.Users.Domain.UserId (UserId)
import Core.Users.Domain.UserType (UserType(..))
import Core.Users.Domain.User (User)
import Core.Users.MonadClasses.Repository
  (UsersRepository(getGroupsOwnedByFromRepo, getGroupsParticipatedByFromRepo))
import Core.Common.Operators ((*>>))

type Dependencies m = (UsersRepository m)
getAllGroups :: Dependencies m => UserId 'Single -> m [User 'Group]
getAllGroups = getGroupsOwnedByFromRepo *>> getGroupsParticipatedByFromRepo
