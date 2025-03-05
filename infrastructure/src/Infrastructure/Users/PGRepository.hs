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
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeApplications #-}

module Infrastructure.Users.PGRepository
  ( UsersRepositoryT(..)
  , createUsersTable
  , createBudgetsTable
  , createOtherUserIdsTable
  ) where

import Database.PostgreSQL.Simple (ToRow, Only (..), FromRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Data.UUID (UUID)
import Optics ((^.), (%), (^?), (^..))

import Data.Maybe (fromJust)
import Data.Typeable (type (:~:)(Refl), Typeable, eqT)
import Control.Monad (void, forM_, forM)
import Control.Monad.IO.Class (MonadIO)
import GHC.Generics (Generic)

import SmartPrimitives.TextLenRange (TextLenRange)
import Core.Common.Operators ((^^?))
import Core.Common.Domain.RubKopecks (RubKopecks(RubKopecks))
import Core.Users.Budget.Domain.Budget (Budget(..))
import Core.Users.Domain.User (User(..), UserData(..), SomeUser(SomeUser))
import Core.Users.Domain.Primitives (Username(..))
import Core.Users.Domain.UserId (UserId (UserId), SomeUserId(SomeUserId))
import Core.Users.Domain.UserType (UserType(..))
import Core.Users.MonadClasses.Repository (UsersRepository(..))
import Infrastructure.Common.Persistence
  (MonadConnReader, execute, executeMany, withTransaction, query, queryMaybe, execute_)

newtype UsersRepositoryT m a = UsersRepositoryT
  { runUsersRepositoryT :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadConnReader)

instance (MonadIO m, MonadConnReader m) => UsersRepository (UsersRepositoryT m) where
  addUserToRepo = addUserToDb
  getSomeUserFromRepo = getSomeUserFromDb
  userExistsInRepo = userExistsInDb
  tryApplyBudgetDeltaToUserInRepo = tryApplyBudgetDeltaToUserInDb
  trySetUserBudgetAmountInRepo = trySetUserBudgetAmountInDb

  getUserFromRepo :: forall t. Typeable t => UserId t -> UsersRepositoryT m (Maybe (User t))
  getUserFromRepo = case eqT @t @'Single of
    Just Refl -> getUserSingleFromDb
    Nothing -> case eqT @t @'Group of
      Just Refl -> getUserGroupFromDb
      Nothing -> error "unreachable"

addUserToDb :: (MonadIO m, MonadConnReader m) => User t -> m ()
addUserToDb user = do
  let (dbUser, mDbBudget, otherUserIdRelations) = toDb user
  withTransaction $ do
    void $ execute [sql|
      INSERT INTO users (userId, username, ownerId, isGroup) VALUES (?, ?, ?, ?)
    |] dbUser
    void $ executeMany [sql|
      INSERT INTO otherUserIds (userGroupId, userSingleId) VALUES (?, ?)
    |] otherUserIdRelations
    forM_ mDbBudget $ execute [sql|
      INSERT INTO budgets (userId, amount, lowerBound) VALUES (?, ?, ?)
    |]

getSomeUserFromDb :: (MonadIO m, MonadConnReader m) => SomeUserId -> m (Maybe SomeUser)
getSomeUserFromDb (SomeUserId userId) = withTransaction $ do
  mDbUserJoinBudget <- fmap unjoin <$> queryMaybe [sql|
    SELECT userId, username, ownerId, isGroup
    FROM users NATURAL JOIN budgets WHERE userId = ?
  |] (Only userId)
  forM mDbUserJoinBudget $ \(dbUser, mDbBudget) -> do
    otherUserIdRelations <- query [sql|
      SELECT userGroupId, userSingleId
      FROM otherUserIds WHERE userGroupId = ?
    |] (Only userId)
    return $ toDomainSome dbUser mDbBudget otherUserIdRelations

getUserSingleFromDb :: (MonadIO m, MonadConnReader m) => UserId 'Single -> m (Maybe (User 'Single))
getUserSingleFromDb (UserId userId) =
  fmap (uncurry toDomainSingle . unjoin) <$> queryMaybe [sql|
    SELECT userId, username, amount, lowerBound, NULL, FALSE
    FROM users WHERE userId = ?
  |] (Only userId)

getUserGroupFromDb :: (MonadIO m, MonadConnReader m) => UserId 'Group -> m (Maybe (User 'Group))
getUserGroupFromDb (UserId userId) = withTransaction $ do
  mDbUserJoinBudget <- fmap unjoin <$> queryMaybe [sql|
    SELECT userId, username, isGroup, budgetAmount, budgetLowerBound, ownerId
    FROM users WHERE userId = ?
  |] (Only userId)
  forM mDbUserJoinBudget $ \(dbUser, mDbBudget) -> do
    otherUserIdRelations <- query [sql|
      SELECT userGroupId, userSingleId
      FROM otherUserIds WHERE userGroupId = ?
    |] (Only userId)
    return $ toDomainGroup dbUser mDbBudget otherUserIdRelations

userExistsInDb :: (MonadIO m, MonadConnReader m)
               => SomeUserId -> m Bool
userExistsInDb (SomeUserId userId) =
  fromOnly . head <$> query [sql|
    SELECT EXISTS(SELECT userId FROM users WHERE userId = ?)
  |] (Only userId)

tryApplyBudgetDeltaToUserInDb :: (MonadIO m, MonadConnReader m)
                              => SomeUserId -> RubKopecks -> m (Maybe RubKopecks)
tryApplyBudgetDeltaToUserInDb (SomeUserId userId) (RubKopecks kopecks) =
  fmap (RubKopecks . fromOnly) <$> queryMaybe [sql|
    UPDATE budgets
    SET amount = amount + ?
    WHERE userId = ?
    RETURNING amount
  |] (kopecks, userId)

trySetUserBudgetAmountInDb :: (MonadIO m, MonadConnReader m)
                           => SomeUserId -> RubKopecks -> m Bool
trySetUserBudgetAmountInDb (SomeUserId userId) (RubKopecks kopecks) =
  (0 <) <$> execute [sql|
    UPDATE budgets
    SET amount = ?
    WHERE userId = ?
  |] (kopecks, userId)

toDb :: User t -> (DbUser, Maybe DbBudget, [OtherUserIdRelation])
toDb user =
  let
    userId = user ^. #userId % #value
    username = user ^. #data % #username % #value
    mAmount = user ^^? #data % #mBudget % #amount
    mLowerBound = user ^^? #data % #mBudget % #mLowerBound
    ownerId = user ^? #mOwnerId % #value
    isGroup = case user of UserGroup{} -> True; _ -> False
    otherUserIdRelations = OtherUserIdRelation userId <$> user ^.. #otherUserIds % #value
  in (DbUser{..}, DbBudget userId <$> mAmount <*> pure mLowerBound, otherUserIdRelations)

toDomainSingle :: DbUser -> Maybe DbBudget -> User 'Single
toDomainSingle DbUser{..} mDbBudget = UserSingle
  { userSingleId = UserId userId
  , userSingleData = UserData
    { username = Username username
    , mBudget = toDomainBudget <$> mDbBudget
    }
  }

toDomainSome :: DbUser -> Maybe DbBudget -> [OtherUserIdRelation] -> SomeUser
toDomainSome dbUser@DbUser{..} mDbBudget otherUserIdRelations
  | isGroup = SomeUser $ toDomainGroup dbUser mDbBudget otherUserIdRelations
  | otherwise = SomeUser $ toDomainSingle dbUser mDbBudget

toDomainBudget :: DbBudget -> Budget
toDomainBudget DbBudget{..} = Budget
  { amount = RubKopecks amount
  , mLowerBound = RubKopecks <$> mLowerBound
  }

toDomainGroup :: DbUser -> Maybe DbBudget -> [OtherUserIdRelation] -> User 'Group
toDomainGroup DbUser{..} mDbBudget otherUserIds = UserGroup
  { userGroupId = UserId userId
  , ownerId = UserId $ fromJust ownerId
  , otherUserIds = UserId . userSingleId <$> otherUserIds
  , userGroupData = UserData
    { username = Username username
    , mBudget = toDomainBudget <$> mDbBudget
    }
  }

unjoin :: DbUserJoinMaybeBudget -> (DbUser, Maybe DbBudget)
unjoin DbUserJoinMaybeBudget{..} =
  (DbUser{..}, DbBudget userId <$> mBudgetAmount <*> pure mBudgetLowerBound)

data DbUserJoinMaybeBudget = DbUserJoinMaybeBudget
  { userId :: UUID
  , username :: TextLenRange 2 50
  , isGroup :: Bool
  , mBudgetAmount :: Maybe Integer
  , mBudgetLowerBound :: Maybe Integer
  , ownerId :: Maybe UUID
  } deriving (Generic, FromRow)

data DbUser = DbUser
  { userId :: UUID
  , username :: TextLenRange 2 50
  , ownerId :: Maybe UUID
  , isGroup :: Bool
  } deriving (Generic, ToRow, FromRow)

createUsersTable :: (MonadIO m, MonadConnReader m) => m ()
createUsersTable = void $ execute_ [sql|
  CREATE TABLE IF NOT EXISTS users
  ( userId UUID PRIMARY KEY NOT NULL
  , username VARCHAR(50) NOT NULL
  , ownerId UUID REFERENCES users(userId)
  , isGroup BOOLEAN NOT NULL
  )
|]

data DbBudget = DbBudget
  { userId :: UUID
  , amount :: Integer
  , mLowerBound :: Maybe Integer
  } deriving (Generic, ToRow, FromRow)

createBudgetsTable :: (MonadIO m, MonadConnReader m) => m ()
createBudgetsTable = void $ execute_ [sql|
  CREATE TABLE IF NOT EXISTS budgets
  ( userId UUID PRIMARY KEY NOT NULL REFERENCES users(userId)
  , amount INTEGER NOT NULL
  , lowerBound INTEGER
  )
|]

data OtherUserIdRelation = OtherUserIdRelation
  { userGroupId :: UUID
  , userSingleId :: UUID
  } deriving (Generic, ToRow, FromRow)

createOtherUserIdsTable :: (MonadIO m, MonadConnReader m) => m ()
createOtherUserIdsTable = void $ execute_ [sql|
  CREATE TABLE IF NOT EXISTS otherUserIds
  ( userGroupId UUID NOT NULL REFERENCES users(userId)
  , userSingleId UUID NOT NULL REFERENCES users(userId)
  , PRIMARY KEY (userGroupId, userSingleId)
  )
|]
