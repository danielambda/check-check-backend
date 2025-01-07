{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Receipts.API (ReceiptsAPI, receiptsServer) where

import Servant ((:>), Capture, Get, JSON, ServerT)
import Database.PostgreSQL.Simple (Connection)

import Control.Monad.IO.Class (MonadIO)

import Receipts.Types
import Receipts.Fetching (fetchReceiptItems, MonadEnvReader)
import Receipts.Persistence (getReceiptItemsFromDb, addReceiptItemsToDb)

type ReceiptsAPI = "receipts" :> Capture "qr" String :> Get '[JSON] Receipt

receiptsServer :: (MonadIO m, MonadEnvReader m) => Connection -> ServerT ReceiptsAPI m
receiptsServer conn = getReceiptFromQr
  where
    getReceiptFromQr :: (MonadIO m, MonadEnvReader m) => String -> m Receipt
    getReceiptFromQr qr = Receipt <$> do
      receiptItemsFromDb <- getReceiptItemsFromDb conn qr
      if null receiptItemsFromDb then do
        fetchedItems <- fetchReceiptItems qr
        addReceiptItemsToDb conn qr fetchedItems
        return fetchedItems
      else
        return receiptItemsFromDb
