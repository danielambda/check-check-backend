{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Groups.Persistence
  ( addGroupToDb, getGroupFromDb
  , createGroupsTable
  , createGroupsBudgetLogsTable
  , createGoodsStorageEntiresTable
  , increaseGoodsQuantityDb, decreaseGoodsQuantityDb
  , getStorageEntriesFromDb
  ) where

import Control.Monad (void)

import Shared.Persistence
  ( MonadConnPoolReader, sql, Only (Only)
  , queryMaybe, querySingleField
  , execute_, executeMany, query
  )
import Groups.Types (Group(Group, name, budget), GroupId (GroupId))
import Goods.Types (GoodsId, Goods (Goods))
import Shared.Types.Positive (Positive)

addGroupToDb :: MonadConnPoolReader m => Group -> m GroupId
addGroupToDb Group{ name, budget } = querySingleField [sql|
  INSERT INTO groups (name, budget) VALUES (?, ?) RETURNING id
|] (name, budget)

getGroupFromDb :: MonadConnPoolReader m => GroupId -> m (Maybe Group)
getGroupFromDb (GroupId groupId) = queryMaybe [sql|
  SELECT name, budget FROM groups WHERE id = ?
|] (Only groupId)

createGroupsTable :: MonadConnPoolReader m => m ()
createGroupsTable = void $ execute_ [sql|
  CREATE TABLE groups
  ( id UUID NOT NULL DEFAULT uuid_generate_v4()
  , name TEXT NOT NULL
  , budget BIGINT NOT NULL
  , PRIMARY KEY (id)
  )
|]

increaseGoodsQuantityDb :: MonadConnPoolReader m
                        => [(GoodsId, Positive Double)] -> GroupId -> m ()
increaseGoodsQuantityDb deltasList groupId = void $ executeMany [sql|
  INSERT INTO goods_storage_entries (group_id, goods_id, quantity) VALUES (?, ?, ?)
  ON CONFLICT (group_id, goods_id)
  DO UPDATE SET quantity = goods_storage_entries.quantity + EXCLUDED.quantity
|] (addGroupId <$> deltasList)
  where addGroupId (a, b) = (groupId, a, b)

decreaseGoodsQuantityDb :: MonadConnPoolReader m
                        => [(GoodsId, Positive Double)] -> GroupId -> m ()
decreaseGoodsQuantityDb deltasList groupId = void $ executeMany [sql|
  UPDATE goods_storage_entries
  SET quantity = GREATEST(0, quantity - upd.delta)
  FROM (VALUES (?, ?, ?)) as upd(group_id, goods_id, delta)
  WHERE goods_storage_entries.group_id::TEXT = upd.group_id
    AND goods_storage_entries.goods_id::TEXT = upd.goods_id
|] (addGroupId <$> deltasList)
  where addGroupId (a, b) = (groupId, a, b)

getStorageEntriesFromDb :: MonadConnPoolReader m
                        => GroupId -> m [(Goods, Double)]
getStorageEntriesFromDb groupId = fmap structurize <$> query [sql|
  SELECT goods.name, goods.units, entries.quantity
  FROM goods_storage_entries entries
  JOIN goods ON entries.goods_id = goods.id
  WHERE entries.group_id = ?
|] (Only groupId)
  where structurize (name, units, quantity) = (Goods name units, quantity)

createGoodsStorageEntiresTable :: MonadConnPoolReader m => m ()
createGoodsStorageEntiresTable = void $ execute_ [sql|
  CREATE TABLE goods_storage_entries
  ( group_id UUID NOT NULL REFERENCES groups(id)
  , goods_id UUID NOT NULL REFERENCES goods(id)
  , quantity REAL NOT NULL CHECK (quantity >= 0)
  , PRIMARY KEY (group_id, goods_id)
  )
|]

createGroupsBudgetLogsTable :: MonadConnPoolReader m => m ()
createGroupsBudgetLogsTable = void $ execute_ [sql|
  CREATE TYPE GROUPS_BUDGET_EVENT
    AS ENUM ('Created', 'Fundraising', 'Purchase');

  CREATE TABLE groups_budget_logs
  ( id UUID NOT NULL DEFAULT uuid_generate_v4()
  , group_id UUID NOT NULL REFERENCES groups(id)
  , event GROUPS_BUDGET_EVENT NOT NULL
  , delta BIGINT NOT NULL
  , created_at TIMESTAMP NOT NULL DEFAULT NOW()
  , PRIMARY KEY (id)
  )
|]
