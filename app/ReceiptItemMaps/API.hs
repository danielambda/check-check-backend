{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}

module ReceiptItemMaps.API (ReceiptItemMapsAPI, receiptItemMapsServer) where

import Servant (JSON, (:>), Get, Capture, QueryParam, HasServer (ServerT), (:<|>) ((:<|>)), PostNoContent, ReqBody, NoContent (NoContent))
import Data.Text (Text)
import Data.Aeson (FromJSON)

import GHC.Generics (Generic)

import Shared.Persistence (MonadConnPoolReader)
import Shared.Types.Positive (Positive)
import Groups.Types (GroupId)
import Goods.Types (GoodsId)
import ReceiptItemMaps.Persistence (addReceiptItemMapToDb, getReceiptItemMapFromDb)
import ReceiptItemMaps.Types (ReceiptItemMap (ReceiptItemMap))

type ReceiptItemMapsAPI = "receipt-item-maps"
  :> ( ReqBody '[JSON] CreateReceipItemMapReqBody :> PostNoContent
  :<|> Capture "name" Text :> QueryParam "groupId" GroupId :> Get '[JSON] (Maybe ReceiptItemMap)
  )

receiptItemMapsServer :: MonadConnPoolReader m => ServerT ReceiptItemMapsAPI m
receiptItemMapsServer
  =    createReceiptItemMap
  :<|> getReceiptItemMap
  where
    createReceiptItemMap :: MonadConnPoolReader m => CreateReceipItemMapReqBody -> m NoContent
    createReceiptItemMap CreateReceipItemMapReqBody{ groupId, name, goodsId, quantity } = do
      let receiptItemMap = ReceiptItemMap groupId name goodsId quantity
      NoContent <$ addReceiptItemMapToDb receiptItemMap

    getReceiptItemMap :: MonadConnPoolReader m => Text -> Maybe GroupId -> m (Maybe ReceiptItemMap)
    getReceiptItemMap = flip getReceiptItemMapFromDb

data CreateReceipItemMapReqBody = CreateReceipItemMapReqBody
  { groupId :: Maybe GroupId
  , name :: Text
  , goodsId :: GoodsId
  , quantity :: Positive Double
  } deriving (Generic, FromJSON)
