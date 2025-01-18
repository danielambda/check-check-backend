{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}

module Goods.API (GoodsAPI, goodsServer) where

import Servant (PostCreated, (:>), JSON, ReqBody, Capture, (:<|>) ((:<|>)), Get, ServerT)
import Data.Text (Text)
import Data.Aeson (FromJSON)

import GHC.Generics (Generic)

import Shared.Persistence (MonadConnPoolReader)
import Goods.Types (GoodsId, Goods (Goods), GoodsUnits)
import Goods.Persistence (addSingleGoodsToDb, getGoodsFromDb)

type GoodsAPI = "goods"
  :> ( ReqBody '[JSON] CreateGoodsReqBody :> PostCreated '[JSON] GoodsId
  :<|> Capture "goodsId" GoodsId :>  Get '[JSON] (Maybe Goods)
  )

goodsServer :: MonadConnPoolReader m
            => ServerT GoodsAPI m
goodsServer
  =    createGoods
  :<|> getGoods
  where
    createGoods :: MonadConnPoolReader m
                => CreateGoodsReqBody -> m GoodsId
    createGoods CreateGoodsReqBody{ name, units } = do
      let goods = Goods name units
      addSingleGoodsToDb goods

    getGoods :: MonadConnPoolReader m
             => GoodsId -> m (Maybe Goods)
    getGoods = getGoodsFromDb

data CreateGoodsReqBody = CreateGoodsReqBody
  { name :: Text
  , units :: GoodsUnits
  } deriving (Generic, FromJSON)
