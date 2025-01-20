{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Receipts.API (ReceiptsAPI, receiptsServer) where

import Servant ((:>), ServerT)

import Shared.Persistence (MonadConnPoolReader)
import Receipts.API.GetReceipt (GetReceipt, getReceipt)

type ReceiptsAPI = "receipts" :> GetReceipt

receiptsServer :: MonadConnPoolReader m => ServerT ReceiptsAPI m
receiptsServer = getReceipt
