{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Receipts.API (ReceiptsAPI, receiptsServer) where

import Servant ((:>), Capture, Get, JSON, ServerT)

import Data.List (sortOn)

import Common.Persistence (MonadConnPoolReader)
import Receipts.Types (Receipt(Receipt), ReceiptItem (quantity, name))
import Receipts.Fetching (fetchReceiptItems, MonadEnvReader)
import Receipts.Persistence (getReceiptItemsFromDb, addReceiptItemsToDb)

type ReceiptsAPI = "receipts" :>
  Capture "qr" String :> Get '[JSON] Receipt

receiptsServer :: (MonadEnvReader m, MonadConnPoolReader m) =>
                  ServerT ReceiptsAPI m
receiptsServer = getReceiptFromQr
  where
    getReceiptFromQr :: (MonadEnvReader m, MonadConnPoolReader m) =>
                        String -> m Receipt
    getReceiptFromQr qr = Receipt <$> do
      receiptItemsFromDb <- getReceiptItemsFromDb qr
      if null receiptItemsFromDb then do
        fetchedItems <- fetchReceiptItems qr
        let items = mergeEponymousItems fetchedItems
        addReceiptItemsToDb qr items
        return items
      else
        return receiptItemsFromDb
      where
        mergeEponymousItems :: [ReceiptItem] -> [ReceiptItem]
        mergeEponymousItems
          = foldr merge []
          . sortOn name
          where
            merge item [] = [item]
            merge item (headItem:items)
              | name item == name headItem =
                  let newHeadItem = headItem{ quantity = quantity headItem + quantity item }
                  in newHeadItem:items
              | otherwise = item:headItem:items


