{-# LANGUAGE DataKinds #-}

module Core.Users.CreateExistingSingle (createExistingSingle) where

import Core.Users.Domain.UserId (UserId)
import Core.Users.Domain.UserType (UserType(Single))
import Core.Users.Domain.User (User (UserSingle), UserData (UserData))
import Core.Users.MonadClasses.Repository (UsersRepository (addUserToRepo))
import Core.Users.Domain.Primitives (Username)
import Core.Common.Operators ((*>>))

createExistingSingle :: UsersRepository m
                     => UserId 'Single -> Username -> m (User 'Single)
createExistingSingle userId username =
  addUserToRepo *>> return $ UserSingle userId $ UserData username Nothing
