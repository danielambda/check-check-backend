{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Receipts.API (ReceiptsAPI, receiptsServer) where

import Servant ((:>), ServerT)

import Receipts.GetReceipt.Endpoint (GetReceipt, getReceipt)
import Receipts.Infrastructure.Fetching (ReceiptsFetchingT (runReceiptsFetchingT))
import Receipts.Infrastructure.Persistence (ReceiptsRepositoryT(runReceiptsRepositoryT))
import Control.Monad.IO.Class (MonadIO)

type ReceiptsAPI = "receipts" :> GetReceipt

receiptsServer :: (MonadIO m) => ServerT ReceiptsAPI m
receiptsServer = getReceipt'
  where
    getReceipt'
      = runReceiptsFetchingT
      . runReceiptsRepositoryT
      . getReceipt
