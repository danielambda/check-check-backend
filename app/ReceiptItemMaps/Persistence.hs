{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module ReceiptItemMaps.Persistence
  ( createReceiptItemMapsTable, createReceiptItemMapsModerationEntriesTable
  ) where

import Control.Monad (void)

import Common.Persistence (sql, MonadConnPoolReader, execute_)

-- TODO TODO TODO
-- TODO TODO TODO
-- TODO TODO TODO
-- TODO TODO TODO
-- TODO TODO TODO
-- TODO this logic is not necessary yet
-- TODO this logic is not necessary yet
-- TODO this logic is not necessary yet
-- TODO this logic is not necessary yet
-- TODO TODO TODO
-- TODO TODO TODO
-- TODO TODO TODO
-- TODO TODO TODO
-- TODO TODO TODO

createReceiptItemMapsTable :: MonadConnPoolReader m => m ()
createReceiptItemMapsTable = void $ execute_ [sql|
  CREATE TABLE receipt_items_maps
  ( group_id UUID REFERENCES groups(id)
  , name TEXT NOT NULL
  , goods_id UUID NOT NULL REFERENCES goods(id)
  , created_at TIMESTAMP DEFAULT NOW()
  , PRIMARY KEY (group_id, name)
  )
|]

createReceiptItemMapsModerationEntriesTable :: MonadConnPoolReader m => m ()
createReceiptItemMapsModerationEntriesTable = void $ execute_ [sql|
  CREATE TYPE RECEIPT_ITEMS_MAPS_MODERATION_STATUS
    AS ENUM ('Pending', 'Accepted', 'Rejected');

  CREATE TABLE receipt_items_maps_moderation_entries
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
