{-# LANGUAGE QuasiQuotes #-}

module ReceiptItemAssociations.Persistence
  ( createReceiptItemAssociationsTable
  , createReceiptItemAssociationsModerationEntriesTable
  ) where

import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as PG (execute_)

import Control.Monad (void)

createReceiptItemAssociationsTable :: Connection -> IO ()
createReceiptItemAssociationsTable conn = void $ PG.execute_ conn [sql|
  CREATE TABLE receipt_items_associations
  ( name TEXT NOT NULL
  , group_id UUID REFERENCES groups(id)
  , goods_id UUID NOT NULL REFERENCES goods(id)
  , created_at TIMESTAMP DEFAULT NOW()
  , PRIMARY KEY (name, group_id)
  )
|]

createReceiptItemAssociationsModerationEntriesTable :: Connection -> IO ()
createReceiptItemAssociationsModerationEntriesTable conn = void $ PG.execute_ conn [sql|
  CREATE TYPE RECEIPT_ITEMS_ASSOCIATIONS_MODERATION_STATUS
    AS ENUM ('pending', 'accepted', 'rejected');

  CREATE TABLE receipt_items_associations_moderation_entries
  ( id UUID NOT NULL
  , receipt_item_name TEXT NOT NULL
  , user_id UUID NOT NULL REFERENCES users(id)
  , group_id UUID NOT NULL REFERENCES groups(id)
  , status RECEIPT_ITEMS_ASSOCIATIONS_MODERATION_STATUS NOT NULL
  , created_at TIMESTAMP NOT NULL DEFAULT NOW()
  , updated_at TIMESTAMP
  , updated_by UUID
  )
|]
