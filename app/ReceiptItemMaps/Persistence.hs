{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module ReceiptItemMaps.Persistence
  ( createReceiptItemMapsTable
  , addReceiptItemMapToDb, getReceiptItemMapFromDb
  , createReceiptItemMapsModerationEntriesTable
  ) where

import Control.Monad (void)

import Shared.Persistence (sql, MonadConnPoolReader, execute_, execute, queryMaybe)
import ReceiptItemMaps.Types (ReceiptItemMap (ReceiptItemMap, quantity, goodsId, name, groupId))
import Groups.Types (GroupId)
import Data.Text (Text)

addReceiptItemMapToDb :: MonadConnPoolReader m => ReceiptItemMap -> m ()
addReceiptItemMapToDb (ReceiptItemMap{ groupId, name, goodsId, quantity })
  = void $ execute [sql|
    INSERT INTO receipt_item_maps (group_id, name, goods_id, quantity) VALUES (?, ?, ?, ?)
  |] (groupId, name, goodsId, quantity)

getReceiptItemMapFromDb :: MonadConnPoolReader m => Maybe GroupId -> Text -> m (Maybe ReceiptItemMap)
getReceiptItemMapFromDb groupId name = queryMaybe [sql|
  SELECT (group_id, name, goods_id, quantity) FROM receipt_item_maps
  WHERE group_id = ? AND name = ?
|] (groupId, name)

createReceiptItemMapsTable :: MonadConnPoolReader m => m ()
createReceiptItemMapsTable = void $ execute_ [sql|
  CREATE TABLE receipt_item_maps
  ( group_id UUID REFERENCES groups(id)
  , name TEXT NOT NULL
  , goods_id UUID NOT NULL REFERENCES goods(id)
  , quantity REAL NOT NULL CHECK (quantity > 0)
  , created_at TIMESTAMP DEFAULT NOW()
  , PRIMARY KEY (group_id, name)
  )
|]

createReceiptItemMapsModerationEntriesTable :: MonadConnPoolReader m => m ()
createReceiptItemMapsModerationEntriesTable = void $ execute_ [sql|
  CREATE TYPE RECEIPT_ITEMS_MAPS_MODERATION_STATUS
    AS ENUM ('Pending', 'Accepted', 'Rejected');

  CREATE TABLE receipt_item_maps_moderation_entries
  ( id UUID NOT NULL
  , receipt_item_name TEXT NOT NULL
  , user_id UUID NOT NULL REFERENCES users(id)
  , group_id UUID NOT NULL REFERENCES groups(id)
  , status RECEIPT_ITEMS_MAPS_MODERATION_STATUS NOT NULL
  , created_at TIMESTAMP NOT NULL DEFAULT NOW()
  , updated_at TIMESTAMP
  , updated_by UUID
  )
|]
