{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Core.Users.CreateSingle
  ( Data(..)
  , Dependencies
  , createSingle
  ) where

import SmartPrimitives.TextLenRange (TextLenRange)
import Core.Common.MonadClasses.MonadUUID (MonadUUID)
import Core.Common.Operators ((*>>))
import Core.Users.MonadClasses.Repository (UsersRepository(addUserToRepo))
import Core.Users.Domain.UserType (UserType(Single))
import Core.Users.Domain.User (User, newUserSingle, UserData(..))
import Core.Users.Domain.Primitives (Username(..))

newtype Data = Data
  { username :: TextLenRange 2 50 }

type Dependencies m = (MonadUUID m, UsersRepository m)
createSingle :: Dependencies m => Data -> m (User 'Single)
createSingle Data{ username } =
  let userData = UserData
        { username = Username username
        , mBudget = Nothing
        }
  in newUserSingle userData >>=
    (addUserToRepo *>> return)
