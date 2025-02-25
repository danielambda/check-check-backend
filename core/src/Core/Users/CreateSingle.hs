{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ConstraintKinds #-}

module Core.Users.CreateSingle
  ( Data(..), CreateBudgetData(..), Dependencies
  , createSingle
  ) where

import SmartPrimitives.TextLenRange (TextLenRange)
import Core.Common.MonadClasses.MonadUUID (MonadUUID)
import Core.Common.Operators ((*>>))
import Core.Users.Domain.UserType (UserType(Single))
import Core.Users.Domain.User (User, newUserSingle, UserData(..))
import Core.Users.MonadClasses.Repository (UsersRepository(addUserToRepo))
import Core.Users.Domain.Primitives (Username(..))
import Core.Users.Common.CreateBudgetData (CreateBudgetData(..), mkBudget)

data Data = Data
  { name :: TextLenRange 2 50
  , mBudgetData :: Maybe CreateBudgetData
  }

type Dependencies m = (MonadUUID m, UsersRepository m)
createSingle :: Dependencies m => Data -> m (User 'Single)
createSingle Data{ name, mBudgetData } =
  let userData = UserData
        { username = Username name
        , mBudget = mkBudget <$> mBudgetData
        }
  in newUserSingle userData >>=
    (addUserToRepo *>> return)
