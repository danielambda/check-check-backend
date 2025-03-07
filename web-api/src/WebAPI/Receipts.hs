{-# LANGUAGE FlexibleContexts #-}

module WebAPI.Receipts (receiptsServer) where

import Servant (ServerT)

import CheckCheck.Contracts.Receipts (ReceiptsAPI)
import WebAPI.Receipts.Get (getReceipt)
import qualified WebAPI.Receipts.Get as Get (Dependencies)

receiptsServer :: (Get.Dependencies m) => ServerT ReceiptsAPI m
receiptsServer = getReceipt
