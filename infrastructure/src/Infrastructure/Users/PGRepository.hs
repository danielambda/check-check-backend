{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Infrastructure.Users.PGRepository
  ( UsersRepositoryT(..)
  , createUsersTable
  , createOtherUserIdsTable
  ) where

import Database.PostgreSQL.Simple (ToRow, Only (..), FromRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Data.UUID (UUID)
import Optics ((^.), (%), (^?), (^..))

import Data.Maybe (fromJust)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import GHC.Generics (Generic)

import SmartPrimitives.TextLenRange (TextLenRange)
import Core.Common.Operators ((^^?))
import Core.Users.MonadClasses.Repository (UsersRepository(..))
import Core.Users.Domain.User (User(..), UserData(..), SomeUser(SomeUser))
import Core.Users.Domain.Primitives (Username(..))
import Core.Users.Domain.UserId (UserId (UserId), SomeUserId(SomeUserId))
import Core.Users.Domain.UserType (UserType(..))
import Core.Users.Budget.Domain.Budget (Budget(Budget))
import Infrastructure.Common.Persistence
  (MonadConnReader, execute, executeMany, withTransaction, query, queryMaybe, execute_)
import Core.Common.Domain.RubKopecks (RubKopecks(RubKopecks))

newtype UsersRepositoryT m a = UsersRepositoryT
  { runUsersRepositoryT :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadConnReader)

instance (MonadIO m, MonadConnReader m) => UsersRepository (UsersRepositoryT m) where
  addUserToRepo = addUserToDb
  getUserSingleFromRepo = getUserSingleFromDb
  getUserGroupFromRepo = getUserGroupFromDb
  userExistsInRepo = userExistsInDb
  tryApplyBudgetDeltaToUserInRepo = tryApplyBudgetDeltaToUserInDb

addUserToDb :: (MonadIO m, MonadConnReader m)
            => User t -> m ()
addUserToDb user = do
  let (dbUser, otherUserIdRelations) = toDb user
  withTransaction $ do
    void $ execute [sql|
      INSERT INTO users (userId, username, budgetAmount, budgetLowerBound, ownerId, isGroup)
      VALUES (?, ?, ?, ?, ?, ?)
    |] dbUser
    void $ executeMany [sql|
      INSERT INTO otherUserIds (userGroupId, userSingleId) VALUES (?, ?)
    |] otherUserIdRelations

getSomeUserFromDb :: (MonadIO m, MonadConnReader m)
                  => SomeUserId -> m (Maybe SomeUser)
getSomeUserFromDb (SomeUserId (UserId userId)) = withTransaction $ do
  dbUser <- queryMaybe [sql|
    SELECT userId, username, budgetAmount, budgetLowerBound, ownerId, isGroup
    FROM users WHERE userId = ?
  |] (Only userId)
  otherUserIdRelations <- query [sql|
    SELECT userGroupId, userSingleId
    FROM otherUserIds WHERE userGroupId = ?
  |] (Only userId)
  return $ toDomainSome <$> dbUser <*> pure otherUserIdRelations

getUserSingleFromDb :: (MonadIO m, MonadConnReader m)
                    => UserId 'Single -> m (Maybe (User 'Single))
getUserSingleFromDb (UserId userId) =
  fmap toDomainSingle <$> queryMaybe [sql|
    SELECT userId, username, budgetAmount, budgetLowerBound, NULL, FALSE
    FROM users WHERE userId = ?
  |] (Only userId)

getUserGroupFromDb :: (MonadIO m, MonadConnReader m)
                   => UserId 'Group -> m (Maybe (User 'Group))
getUserGroupFromDb (UserId userId) = withTransaction $ do
  dbUser <- queryMaybe [sql|
    SELECT userId, username, budgetAmount, budgetLowerBound, ownerId, isGroup
    FROM users WHERE userId = ?
  |] (Only userId)
  otherUserIdRelations <- query [sql|
    SELECT userGroupId, userSingleId
    FROM otherUserIds WHERE userGroupId = ?
  |] (Only userId)
  return $ toDomainGroup <$> dbUser <*> pure otherUserIdRelations

userExistsInDb :: (MonadIO m, MonadConnReader m)
               => SomeUserId -> m Bool
userExistsInDb (SomeUserId (UserId userId)) =
  fromOnly . head <$> query [sql|
    SELECT EXISTS(SELECT userId FROM users WHERE userId = ?)
  |] (Only userId)

tryApplyBudgetDeltaToUserInDb :: (MonadIO m, MonadConnReader m)
                              => SomeUserId -> RubKopecks -> m (Maybe RubKopecks)
tryApplyBudgetDeltaToUserInDb (SomeUserId (UserId userId)) (RubKopecks kopecks) =
  fmap (RubKopecks . fromOnly) <$> queryMaybe [sql|
    UPDATE users
    SET budgetAmount = budgetAmount + ?
    WHERE userId = ? AND budgetAmount IS NOT NULL
    RETURNING budgetAmount
  |] (kopecks, userId)

toDb :: User t -> (DbUser, [OtherUserIdRelation])
toDb user =
  let
    userId = user ^. #userId % #value
    username = user ^. #data % #username % #value
    budgetAmount = user ^^? #data % #mBudget % #amount
    budgetLowerBound = user ^^? #data % #mBudget % #mLowerBound
    ownerId = user ^? #mOwnerId % #value
    isGroup = case user of UserGroup{} -> True; _ -> False
    otherUserIdRelations = OtherUserIdRelation userId <$> user ^.. #otherUserIds % #value
  in (DbUser{..}, otherUserIdRelations)

toDomainSingle :: DbUser -> User 'Single
toDomainSingle DbUser{..} = UserSingle
  { userSingleId = UserId userId
  , userSingleData = UserData
    { username = Username username
    , mBudget = Budget . RubKopecks <$> budgetAmount <*> pure (RubKopecks <$> budgetLowerBound)
    }
  }

toDomainGroup :: DbUser -> [OtherUserIdRelation] -> User 'Group
toDomainGroup DbUser{..} otherUserIds = UserGroup
  { userGroupId = UserId userId
  , ownerId = UserId $ fromJust ownerId
  , otherUserIds = UserId . userSingleId <$> otherUserIds
  , userGroupData = UserData
    { username = Username username
    , mBudget = Budget . RubKopecks <$> budgetAmount <*> pure (RubKopecks <$> budgetLowerBound)
    }
  }

toDomainSome :: DbUser -> [OtherUserIdRelation] -> SomeUser
toDomainSome dbUser@DbUser{isGroup} otherUserIds
  | isGroup   = SomeUser $ toDomainGroup  dbUser otherUserIds
  | otherwise = SomeUser $ toDomainSingle dbUser

data DbUser = DbUser
  { userId :: UUID
  , username :: TextLenRange 2 50
  , budgetAmount :: Maybe Integer
  , budgetLowerBound :: Maybe Integer
  , ownerId :: Maybe UUID
  , isGroup :: Bool
  } deriving (Generic, ToRow, FromRow)

data OtherUserIdRelation = OtherUserIdRelation
  { userGroupId :: UUID
  , userSingleId :: UUID
  } deriving (Generic, ToRow, FromRow)

createUsersTable :: (MonadIO m, MonadConnReader m) => m ()
createUsersTable = void $ execute_ [sql|
  CREATE TABLE IF NOT EXISTS users
  ( userId UUID PRIMARY KEY NOT NULL
  , username VARCHAR(50) NOT NULL
  , budgetAmount INTEGER
  , budgetLowerBound INTEGER
  , ownerId UUID REFERENCES users(userId)
  , isGroup BOOLEAN NOT NULL
  )
|]

createOtherUserIdsTable :: (MonadIO m, MonadConnReader m) => m ()
createOtherUserIdsTable = void $ execute_ [sql|
  CREATE TABLE IF NOT EXISTS otherUserIds
  ( userGroupId UUID NOT NULL REFERENCES users(userId)
  , userSingleId UUID NOT NULL REFERENCES users(userId)
  , PRIMARY KEY (userGroupId, userSingleId)
  )
|]
