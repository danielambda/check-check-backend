{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module WebAPI.Receipts (ReceiptsAPI, receiptsServer) where

import Servant (ServerT)

import WebAPI.Receipts.Get (GetReceipt, getReceipt)
import qualified WebAPI.Receipts.Get as Get (Dependencies)

type ReceiptsAPI = GetReceipt

receiptsServer :: (Get.Dependencies m) => ServerT ReceiptsAPI m
receiptsServer = getReceipt
