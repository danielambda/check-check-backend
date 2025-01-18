{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Receipts.API (ReceiptsAPI, receiptsServer) where

import Servant ((:>), ServerT)

import Shared.Persistence (MonadConnPoolReader)
import Receipts.API.GetReceipt (GetReceipt, getReceipt)
import qualified Receipts.Fetching as Fetching (MonadEnvReader)

type ReceiptsAPI = "receipts" :> GetReceipt

receiptsServer :: (Fetching.MonadEnvReader m, MonadConnPoolReader m)
               => ServerT ReceiptsAPI m
receiptsServer = getReceipt


