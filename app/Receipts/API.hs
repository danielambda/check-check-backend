{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Receipts.API (ReceiptsAPI, receiptsServer) where

import Servant ((:>), ServerT)

import Control.Monad.IO.Class (MonadIO)

import Receipts.GetReceipt.Endpoint (GetReceipt, getReceipt)
import Receipts.Infrastructure.Fetching (ReceiptsFetchingT (runReceiptsFetchingT))
import Receipts.Infrastructure.Persistence (ReceiptsRepositoryT(runReceiptsRepositoryT))
import Shared.Persistence (MonadConnPoolReader)

type ReceiptsAPI = "receipts" :> GetReceipt

receiptsServer :: (MonadIO m, MonadConnPoolReader m) => ServerT ReceiptsAPI m
receiptsServer = getReceipt'
  where
    getReceipt'
      = runReceiptsRepositoryT
      . runReceiptsFetchingT
      . getReceipt
