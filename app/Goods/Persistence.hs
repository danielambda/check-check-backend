{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Goods.Persistence (addSingleGoodsToDb, createGoodsTable, getGoodsFromDb) where

import Control.Monad (void)

import Common.Persistence (sql, MonadConnPoolReader, execute_, querySingleField, queryMaybe, Only (Only))
import Goods.Types (Goods (Goods, units, name), GoodsId)

addSingleGoodsToDb :: MonadConnPoolReader m
                   => Goods -> m GoodsId
addSingleGoodsToDb Goods{ name, units } = querySingleField [sql|
  INSERT INTO goods (name, units) VALUES (?, ?) RETURNING id
|] (name, units)

getGoodsFromDb :: MonadConnPoolReader m
               => GoodsId -> m (Maybe Goods)
getGoodsFromDb goodsId = queryMaybe [sql|
  SELECT name, units FROM goods WHERE id = ?
|] (Only goodsId)

createGoodsTable :: MonadConnPoolReader m => m ()
createGoodsTable = void $ execute_ [sql|
  CREATE TYPE GOODS_UNITS
    AS ENUM ('Grams', 'Liters', 'Pieces');

  CREATE TABLE goods
  ( id UUID NOT NULL DEFAULT uuid_generate_v4()
  , name TEXT NOT NULL
  , units GOODS_UNITS NOT NULL
  , PRIMARY KEY (id)
  )
|]
