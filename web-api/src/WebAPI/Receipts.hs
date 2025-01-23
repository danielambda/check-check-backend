{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module WebAPI.Receipts (ReceiptsAPI, receiptsServer) where

import Servant ((:>), ServerT)

import Control.Monad.IO.Class (MonadIO)

import WebAPI.Receipts.GetReceipt (GetReceipt, getReceipt)
import Infrastructure.Receipts.Fetching (ReceiptsFetchingT (runReceiptsFetchingT))
import Infrastructure.Receipts.PGRepository (ReceiptsRepositoryT(runReceiptsRepositoryT))
import Infrastructure.Common.Persistence (MonadConnReader)

type ReceiptsAPI = "receipts" :> GetReceipt

receiptsServer :: (MonadIO m, MonadConnReader m) => ServerT ReceiptsAPI m
receiptsServer = getReceipt'
  where
    getReceipt'
      = runReceiptsRepositoryT
      . runReceiptsFetchingT
      . getReceipt
