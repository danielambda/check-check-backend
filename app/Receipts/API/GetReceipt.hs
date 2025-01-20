{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors #-}

module Receipts.API.GetReceipt (GetReceipt, getReceipt) where

import Servant (ServerT, Capture, (:>), JSON, Get)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import Optics (view, (^.), (%))

import Control.Monad (forM_)
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)

import Shared.Persistence (MonadConnPoolReader)
import Shared.Types.Positive (mkPositive)
import Shared.TuppledFieldsOptics (tuppledFields3)
import Receipts.Fetching (MonadEnvReader, fetchReceiptItems, FetchedReceiptItem)
import Receipts.Persistence (getReceiptItemsFromDb, addReceiptItemsToDb, DbReceiptItem)
import Receipts.Domain.Receipt (Receipt, mkReceipt)
import Receipts.Domain.ReceiptItem (ReceiptItem, mkReceiptItem)

type GetReceipt =
  Capture "qr" String :> Get '[JSON] (Maybe ReceiptResp)

getReceipt :: (MonadEnvReader m, MonadConnPoolReader m)
           => ServerT GetReceipt m
getReceipt qr = do
  fmap toResponse <$> actualLogic
  where
    actualLogic = do -- yeah I will rename this someday
      receiptItemsFromDb <- getReceiptItemsFromDb qr
      if null receiptItemsFromDb then do
        fetchedItems <- fetchReceiptItems qr
        let items = mapMaybe fromFetched fetchedItems
        let mReceipt = mkReceipt items
        forM_ mReceipt $ \receipt -> do
          addReceiptItemsToDb qr $ receipt ^. #items
        return mReceipt
      else do
        let items = mapMaybe fromDb receiptItemsFromDb
        let mReceipt = mkReceipt items
        return mReceipt

fromFetched :: FetchedReceiptItem -> Maybe ReceiptItem
fromFetched fetchedItem = do
  let (name, price, quantity) = fetchedItem ^. tuppledFields3 #name #price #quantity
  posPrice <- mkPositive price
  posQuantity <- mkPositive quantity
  return $ mkReceiptItem name posPrice posQuantity

fromDb :: DbReceiptItem -> Maybe ReceiptItem
fromDb dbItem = do
  let (name, price, quantity) = dbItem ^. tuppledFields3 #name #price #quantity
  posPrice <- mkPositive price
  posQuantity <- mkPositive quantity
  return $ mkReceiptItem name posPrice posQuantity

newtype ReceiptResp = ReceiptResp [ReceiptItemResp]
  deriving newtype (ToJSON)

data ReceiptItemResp = ReceiptItemResp
  { name :: Text
  , quantity :: Double
  , price :: Integer
  } deriving (Generic, ToJSON)

toResponse :: Receipt -> ReceiptResp
toResponse = ReceiptResp <$>
  map itemToResponse . view #items
  where
    itemToResponse = ReceiptItemResp
      <$> view #name
      <*> view (#quantity % #value)
      <*> view (#price % #value)

