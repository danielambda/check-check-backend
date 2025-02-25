{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ConstraintKinds #-}

module Core.Users.CreateSingle
  ( Data(..)
  , Dependencies
  , createSingle
  ) where

import SmartPrimitives.TextLenRange (TextLenRange)
import Core.Common.MonadClasses.MonadUUID (MonadUUID)
import Core.Common.Operators ((*>>))
import qualified Core.Users.Budget.Create as CreateBudget (Data(..), create)
import Core.Users.MonadClasses.Repository (UsersRepository(addUserToRepo))
import Core.Users.Domain.UserType (UserType(Single))
import Core.Users.Domain.User (User, newUserSingle, UserData(..))
import Core.Users.Domain.Primitives (Username(..))

data Data = Data
  { name :: TextLenRange 2 50
  , mBudgetData :: Maybe CreateBudget.Data
  }

type Dependencies m = (MonadUUID m, UsersRepository m)
createSingle :: Dependencies m => Data -> m (User 'Single)
createSingle Data{ name, mBudgetData } =
  let userData = UserData
        { username = Username name
        , mBudget = CreateBudget.create <$> mBudgetData
        }
  in newUserSingle userData >>=
    (addUserToRepo *>> return)
