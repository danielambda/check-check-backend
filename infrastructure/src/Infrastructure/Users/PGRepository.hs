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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS_GHC -Wno-x-patrial #-}

module Infrastructure.Users.PGRepository
  ( UsersRepositoryT(..)
  , createUsersTable
  , createBudgetsTable
  , createOtherUserIdsTable
  , createUserContactsTable
  ) where

import Database.PostgreSQL.Simple (ToRow, Only (..), FromRow, SqlError (sqlState, sqlErrorMsg, SqlError))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Data.UUID (UUID)
import Optics ((^.), (%), (^?), (^..))
import qualified Data.HashMap.Strict as HashMap (elems, fromListWith)

import Data.Maybe (fromJust)
import Data.Typeable (type (:~:)(Refl), Typeable, eqT)
import Control.Monad (void, forM_, forM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import GHC.Generics (Generic)

import SmartPrimitives.TextLenRange (TextLenRange)
import SmartPrimitives.TextMaxLen (TextMaxLen)
import Core.Common.Operators ((^^?), (^^.))
import Core.Common.Domain.RubKopecks (RubKopecks(RubKopecks))
import Core.Users.Budget.Domain.Budget (Budget(..))
import Core.Users.Domain.User (User(..), UserData(..), SomeUser(SomeUser))
import Core.Users.Domain.Primitives (Username(..))
import Core.Users.Domain.UserId (UserId (UserId), SomeUserId(SomeUserId))
import Core.Users.Domain.UserType (UserType(..))
import Core.Users.Domain.UserContact (UserContact (..))
import Core.Users.MonadClasses.Repository (UsersRepository(..))
import Infrastructure.Common.Persistence
  ( MonadPG (askConn), execute, executeMany, withTransaction
  , query, queryMaybe, querySingleField, execute_
  )
import Data.Text (Text)
import Control.Exception (try)
import Data.ByteString.Char8 (isInfixOf)
import qualified Data.Text as T (pack)
import Control.Monad.Reader (ReaderT(runReaderT))
import Database.PostgreSQL.Simple.Types (PGArray(..))

newtype UsersRepositoryT m a = UsersRepositoryT
  { runUsersRepositoryT :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadPG)

instance MonadPG m => UsersRepository (UsersRepositoryT m) where
  addUserToRepo = addUserToDb
  getSomeUserFromRepo = getSomeUserFromDb
  userExistsInRepo = userExistsInDb
  updateSomeUserInRepo = updateSomeUserInDb
  getGroupsOwnedByFromRepo = getGroupsOwnedByFromDb
  getGroupsParticipatedByFromRepo = getGroupsParticipatedByFromDb
  getContactsWithUsernamesFromRepo = getContactsWithUsernamesFromDb
  addContactToRepo = addContactToDb
  deleteContactFromRepo = deleteContactFromDb

  getUserFromRepo :: forall t m0. (MonadPG m0, Typeable t)
                  => UserId t -> UsersRepositoryT m0 (Maybe (User t))
  getUserFromRepo = case eqT @t @'Single of
    Just Refl -> getUserSingleFromDb
    Nothing -> case eqT @t @'Group of
      Just Refl -> getUserGroupFromDb
      Nothing -> error "unreachable"

addUserToDb :: MonadPG m => User t -> m (Either Text ())
addUserToDb user = do
  let (dbUser, mDbBudget, otherUserIdRelations) = toDb user
  conn <- askConn
  result <- liftIO $ try $ flip runReaderT conn $ withTransaction $ do
    void $ execute [sql|
      INSERT INTO users (userId, username, ownerId, isGroup) VALUES (?, ?, ?, ?)
    |] dbUser
    void $ executeMany [sql|
      INSERT INTO otherUserIds (userGroupId, userSingleId) VALUES (?, ?)
    |] otherUserIdRelations
    forM_ mDbBudget $ execute [sql|
      INSERT INTO budgets (userId, amount, lowerBound) VALUES (?, ?, ?)
    |]
  case result of
    Right _ -> return $ Right ()
    Left (err :: SqlError) -> handleSqlError err
      where
        handleSqlError SqlError{ sqlState, sqlErrorMsg }
          | sqlState == "23505" = return $ Left $ -- Unique violation error code
              if | "users_pkey" `isInfixOf` sqlErrorMsg -> "userId already exists"
                 | "users_username_key" `isInfixOf` sqlErrorMsg -> "username already exists"
                 | otherwise -> T.pack $ show err
          | otherwise = return $ Left $ T.pack $ show err

getSomeUserFromDb :: MonadPG m => SomeUserId -> m (Maybe SomeUser)
getSomeUserFromDb (SomeUserId userId) = withTransaction $ do
  mDbUserJoinBudget <- fmap unjoin <$> queryMaybe [sql|
    SELECT userId, username, ownerId, isGroup, amount, lowerBound
    FROM users NATURAL LEFT JOIN budgets WHERE userId = ?
  |] (Only userId)
  forM mDbUserJoinBudget $ \(dbUser, mDbBudget) -> do
    otherUserIdRelations <- query [sql|
      SELECT userGroupId, userSingleId
      FROM otherUserIds WHERE userGroupId = ?
    |] (Only userId)
    return $ toDomainSome dbUser mDbBudget otherUserIdRelations

getUserSingleFromDb :: MonadPG m => UserId 'Single -> m (Maybe (User 'Single))
getUserSingleFromDb (UserId userId) =
  fmap (uncurry toDomainSingle . unjoin) <$> queryMaybe [sql|
    SELECT userId, username, NULL, FALSE, amount, lowerBound
    FROM users NATURAL LEFT JOIN budgets WHERE userId = ?
  |] (Only userId)

getUserGroupFromDb :: MonadPG m => UserId 'Group -> m (Maybe (User 'Group))
getUserGroupFromDb (UserId userId) = withTransaction $ do
  mDbUserJoinBudget <- fmap unjoin <$> queryMaybe [sql|
    SELECT userId, username, ownerId, isGroup, amount, lowerBound
    FROM users NATURAL LEFT JOIN budgets WHERE userId = ?
  |] (Only userId)
  forM mDbUserJoinBudget $ \(dbUser, mDbBudget) -> do
    otherUserIdRelations <- query [sql|
      SELECT userGroupId, userSingleId
      FROM otherUserIds WHERE userGroupId = ?
    |] (Only userId)
    return $ toDomainGroup dbUser mDbBudget otherUserIdRelations

userExistsInDb :: MonadPG m => SomeUserId -> m Bool
userExistsInDb (SomeUserId userId) =
  querySingleField [sql|
    SELECT EXISTS(SELECT 1 FROM users WHERE userId = ?)
  |] (Only userId)

updateSomeUserInDb :: MonadPG m => SomeUser -> m ()
updateSomeUserInDb (SomeUser user) = do
  let (dbUser, mDbBudget, otherUserIdRelations) = toDb user
  let userId = user ^^. #userId
  withTransaction $ do
    void $ execute [sql|
      UPDATE users
      SET username = ?, ownerId = ?, isGroup = ?
      WHERE userId = ?
    |] (let DbUser{userId=_, ..} = dbUser in (username, ownerId, isGroup, userId))  -- Replace first param with userId for WHERE clause

    case mDbBudget of
      Nothing ->
        void $ execute [sql| DELETE FROM budgets WHERE userId = ? |] (Only userId)
      Just budget ->
        void $ execute [sql|
          INSERT INTO budgets (userId, amount, lowerBound)
          VALUES (?, ?, ?)
          ON CONFLICT (userId)
          DO UPDATE SET amount = EXCLUDED.amount, lowerBound = EXCLUDED.lowerBound
        |] budget

    void $ execute [sql|
      DELETE FROM otherUserIds
      WHERE userGroupId = ?
      AND userSingleId NOT IN (SELECT unnest(?::uuid[]))
    |] (userId, PGArray $ map userSingleId otherUserIdRelations)

    void $ executeMany [sql|
      INSERT INTO otherUserIds (userGroupId, userSingleId)
      VALUES (?, ?)
      ON CONFLICT (userGroupId, userSingleId) DO NOTHING
    |] otherUserIdRelations

getGroupsOwnedByFromDb :: MonadPG m => UserId 'Single -> m [User 'Group]
getGroupsOwnedByFromDb (UserId ownerId) =
  map (\(x, y, z) -> toDomainGroup x y z) . normalize <$> query [sql|
    SELECT userId, username, amount, lowerBound, userSingleId
    FROM users
    NATURAL LEFT JOIN budgets
    LEFT JOIN otherUserIds ON userId = userGroupId
    WHERE ownerId = ?
  |] (Only ownerId)
  where
    normalize
      = HashMap.elems
      . HashMap.fromListWith merge
      . map processTuple
      where
        merge (group, budget, items1) (_, _, items2) =
          (group, budget, items1 <> items2)
        processTuple (userId, username, amount, lowerBound, userSingleId) =
          ( userId
          , ( DbUser{ isGroup = True, ownerId = Just ownerId, ..}
            , DbBudget ownerId <$> amount <*> lowerBound
            , [OtherUserIdRelation{userGroupId = userId, ..}]
            )
          )

getGroupsParticipatedByFromDb :: MonadPG m => UserId 'Single -> m [User 'Group]
getGroupsParticipatedByFromDb (UserId userSingleId) =
  map (\(x, y, z) -> toDomainGroup x y z) . normalize <$> query [sql|
    SELECT userId, username, amount, lowerBound, ownerId
    FROM users
    NATURAL LEFT JOIN budgets
    LEFT JOIN otherUserIds ON userId = userGroupId
    WHERE ownerId = ?
  |] (Only userSingleId)
  where
    normalize
      = HashMap.elems
      . HashMap.fromListWith merge
      . map processTuple
      where
        merge (group, budget, items1) (_, _, items2) =
          (group, budget, items1 <> items2)
        processTuple (userId, username, amount, lowerBound, ownerId) =
          ( userId
          , ( DbUser{ isGroup = True, ownerId = Just ownerId, ..}
            , DbBudget ownerId <$> amount <*> lowerBound
            , [OtherUserIdRelation{userGroupId = userId, ..}]
            )
          )

getContactsWithUsernamesFromDb :: MonadPG m => UserId 'Single -> m [(UserContact, Username)]
getContactsWithUsernamesFromDb (UserId userId) = map process <$> query [sql|
  SELECT contactUserId, contactName, username
  FROM userContacts
  JOIN users ON users.userId = contactUserId
  WHERE userContacts.userId = ?
|] (Only userId)
  where
    process :: (UUID, Maybe (TextMaxLen 50), TextLenRange 2 50) -> (UserContact, Username)
    process (contactUserId, contactName, username) =
      (toDomainContact DbUserContact{..}, Username username)

addContactToDb :: MonadPG m => UserId 'Single -> UserContact -> m ()
addContactToDb userId userContact = void $ execute [sql|
  INSERT INTO userContacts (userId, contactUserId, contactName) VALUES (?, ?, ?)
|] (toDbContact userId userContact)

deleteContactFromDb :: MonadPG m => UserId 'Single -> UserId 'Single -> m ()
deleteContactFromDb (UserId userId) (UserId contactUserId) = void $ execute [sql|
  DELETE FROM userContacts
  WHERE userId = ? AND contactUserId = ?
|] (userId, contactUserId)

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

toDbContact :: UserId 'Single -> UserContact -> DbUserContact
toDbContact (UserId userId) UserContact{ contactUserId = UserId contactUserId, mContactName = contactName } =
  DbUserContact{..}

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

toDomainContact :: DbUserContact -> UserContact
toDomainContact DbUserContact{..} = UserContact
  { contactUserId = UserId contactUserId
  , mContactName = contactName
  }

unjoin :: DbUserJoinMaybeBudget -> (DbUser, Maybe DbBudget)
unjoin DbUserJoinMaybeBudget{..} =
  (DbUser{..}, DbBudget userId <$> mBudgetAmount <*> pure mBudgetLowerBound)

data DbUserJoinMaybeBudget = DbUserJoinMaybeBudget
  { userId :: UUID
  , username :: TextLenRange 2 50
  , ownerId :: Maybe UUID
  , isGroup :: Bool
  , mBudgetAmount :: Maybe Integer
  , mBudgetLowerBound :: Maybe Integer
  } deriving (Generic, FromRow)

data DbUser = DbUser
  { userId :: UUID
  , username :: TextLenRange 2 50
  , ownerId :: Maybe UUID
  , isGroup :: Bool
  } deriving (Generic, ToRow, FromRow)

createUsersTable :: MonadPG m => m ()
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

createBudgetsTable :: MonadPG m => m ()
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

createOtherUserIdsTable :: MonadPG m => m ()
createOtherUserIdsTable = void $ execute_ [sql|
  CREATE TABLE IF NOT EXISTS otherUserIds
  ( userGroupId UUID NOT NULL REFERENCES users(userId)
  , userSingleId UUID NOT NULL REFERENCES users(userId)
  , PRIMARY KEY (userGroupId, userSingleId)
  )
|]

data DbUserContact = DbUserContact
  { userId :: UUID
  , contactUserId :: UUID
  , contactName :: Maybe (TextMaxLen 50)
  } deriving (Generic, ToRow, FromRow)

createUserContactsTable :: MonadPG m => m ()
createUserContactsTable = void $ execute_ [sql|
  CREATE TABLE IF NOT EXISTS userContacts
  ( userId UUID NOT NULL REFERENCES users(userId)
  , contactUserId UUID NOT NULL REFERENCES users(userId)
  , contactName VARCHAR(50)
  , PRIMARY KEY (userId, contactUserId)
  )
|]
