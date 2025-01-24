{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module WebAPI.Receipts (ReceiptsAPI, receiptsServer) where

import Servant ((:>), ServerT)

import Core.Receipts.MonadClasses.Repository (ReceiptsRepository)
import Core.Receipts.MonadClasses.Fetching (ReceiptsFetching)
import WebAPI.Receipts.GetReceipt (GetReceipt, getReceipt)

type ReceiptsAPI = "receipts" :> GetReceipt

receiptsServer :: (ReceiptsRepository m, ReceiptsFetching m) => ServerT ReceiptsAPI m
receiptsServer = getReceipt
