{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Core.Users.CreateSingle
  ( Data(..)
  , Dependencies
  , createSingle
  ) where

import SmartPrimitives.TextLenRange (TextLenRange)
import Core.Common.MonadClasses.MonadUUID (MonadUUID)
import Core.Users.MonadClasses.Repository (UsersRepository(addUserToRepo))
import Core.Users.Domain.UserType (UserType(Single))
import Core.Users.Domain.User (User (UserSingle), UserData(..))
import Core.Users.Domain.Primitives (Username(..))
import Core.Users.Domain.UserId (UserId)
import Data.Text (Text)

data Data = Data
  { userId :: UserId 'Single
  , username :: TextLenRange 2 50
  }

type Dependencies m = (MonadUUID m, UsersRepository m)
createSingle :: Dependencies m => Data -> m (Either Text (User 'Single))
createSingle Data{ userId, username } = do
  let user = UserSingle userId UserData
        { username = Username username
        , mBudget = Nothing
        }
  addUserToRepo user >>= \case
    Left err -> return $ Left err
    Right _ -> return $ Right user
