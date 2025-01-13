{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ReceiptItemMaps.API (ReceiptItemMapsAPI, receiptItemMapsServer) where

import Servant (JSON, (:>), Get)

type ReceiptItemMapsAPI = "receipt-item-maps" :> Get '[JSON] [String]

receiptItemMapsServer :: a
receiptItemMapsServer = undefined
