{-# LANGUAGE QuasiQuotes #-}

module Goods.Persistence (createGoodsTable) where

import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as PG (execute_)

import Control.Monad (void)

createGoodsTable :: Connection -> IO ()
createGoodsTable conn = void $ PG.execute_ conn [sql|
  CREATE TYPE GOODS_UNITS
    AS ENUM ('grams', 'liters', 'pieces');

  CREATE TABLE goods
  ( id UUID NOT NULL
  , name TEXT NOT NULL
  , units GOODS_UNITS NOT NULL
  , PRIMARY KEY (id)
  )
|]
