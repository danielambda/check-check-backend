{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Users.Persistence (addUserToDb, getUserFromDb, createUsersTable, setUserGroupId) where

import Database.PostgreSQL.Simple (Only (Only, fromOnly))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import Control.Monad (void)

import Shared.Persistence (MonadConnPoolReader, query, queryMaybe, execute, execute_)
import Users.Types (User(User, username, groupId), UserId(UserId))
import Groups.Types (GroupId)

addUserToDb :: MonadConnPoolReader m => User -> m (Either () UserId)
addUserToDb User{ username, groupId } = Right . UserId . fromOnly . head <$> query [sql|
  INSERT INTO users (username, group_id) VALUES (?, ?) RETURNING id
|] (username, groupId)

getUserFromDb :: MonadConnPoolReader m => UserId -> m (Maybe User)
getUserFromDb (UserId userId) = queryMaybe [sql|
  SELECT username, group_id FROM users WHERE id = ?
|] (Only userId)

setUserGroupId :: MonadConnPoolReader m => UserId -> GroupId -> m ()
setUserGroupId userId groupId = void $ execute [sql|
  UPDATE users SET group_id = ? WHERE id = ?
|] (groupId, userId)

createUsersTable :: MonadConnPoolReader m => m ()
createUsersTable = void $ execute_ [sql|
  CREATE TABLE users
  ( id UUID NOT NULL DEFAULT uuid_generate_v4()
  , username TEXT NOT NULL UNIQUE
  , group_id UUID REFERENCES groups(id)
  , created_at TIMESTAMP DEFAULT NOW()
  , PRIMARY KEY (id)
  )
|]
