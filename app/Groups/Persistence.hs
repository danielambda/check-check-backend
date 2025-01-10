{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Groups.Persistence
  ( addGroupToDb, getGroupFromDb
  , createGroupsTable
  , createGroupsBudgetLogsTable
  , createGoodsStorageEntiresTable
  ) where

import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple (Connection, FromRow, Only (fromOnly, Only))
import qualified Database.PostgreSQL.Simple as PG (execute_)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)

import Common.Persistence (MonadConnPoolReader, query, queryMaybe)
import Groups.Types (Group(Group, name, budget), GroupId (GroupId))

getGroupFromDb :: (MonadIO m, MonadConnPoolReader m) => GroupId -> m (Maybe Group)
getGroupFromDb (GroupId groupId) = queryMaybe [sql|
  SELECT name, budget FROM groups WHERE id = ?
|] (Only groupId)

addGroupToDb :: (MonadIO m, MonadConnPoolReader m) => Group -> m GroupId
addGroupToDb Group{ name, budget } = GroupId . fromOnly . head <$> query [sql|
  INSERT INTO groups (name, budget) VALUES (?, ?) RETURNING id
|] (name, budget)

createGroupsTable :: Connection -> IO ()
createGroupsTable conn = void $ PG.execute_ conn [sql|
  CREATE TABLE groups
  ( id UUID NOT NULL DEFAULT uuid_generate_v4()
  , name TEXT NOT NULL
  , budget BIGINT NOT NULL
  , PRIMARY KEY (id)
  )
|]

createGroupsBudgetLogsTable :: Connection -> IO ()
createGroupsBudgetLogsTable conn = void $ PG.execute_ conn [sql|
  CREATE TYPE GROUPS_BUDGET_EVENT
    AS ENUM ('created', 'fundraising', 'purchase');

  CREATE TABLE groups_budget_logs
  ( id UUID NOT NULL DEFAULT uuid_generate_v4()
  , group_id UUID NOT NULL REFERENCES groups(id)
  , event GROUPS_BUDGET_EVENT NOT NULL
  , delta BIGINT NOT NULL
  , created_at TIMESTAMP NOT NULL DEFAULT NOW()
  , PRIMARY KEY (id)
  )
|]

createGoodsStorageEntiresTable :: Connection -> IO ()
createGoodsStorageEntiresTable conn = void $ PG.execute_ conn [sql|
  CREATE TABLE goods_storage_entries
  ( group_id UUID NOT NULL REFERENCES groups(id)
  , goods_id UUID NOT NULL REFERENCES goods(id)
  , quantity REAL NOT NULL
  , PRIMARY KEY (group_id, goods_id)
  )
|]

instance FromRow Group

