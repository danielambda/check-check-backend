{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Receipts.API (ReceiptsAPI, receiptsServer) where

import Servant ((:>), Capture, Get, JSON, ServerT)

import Receipts.Types (Receipt(Receipt))
import Receipts.Fetching (fetchReceiptItems, MonadEnvReader)
import Receipts.Persistence (getReceiptItemsFromDb, addReceiptItemsToDb)
import Common.Persistence (MonadConnPoolReader)

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
        addReceiptItemsToDb qr fetchedItems
        return fetchedItems
      else
        return receiptItemsFromDb
