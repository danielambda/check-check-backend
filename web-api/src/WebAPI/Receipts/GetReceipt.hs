{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoFieldSelectors #-}

module WebAPI.Receipts.GetReceipt (GetReceipt, getReceipt) where

import Servant (ServerT, Capture, (:>), JSON, Get)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import Optics ((^.), (%), toListOf)

import GHC.Generics (Generic)

import qualified Receipts.GetReceipt.Implementation as Impl (getReceipt, Dependencies)
import Receipts.Domain.Receipt (Receipt, receiptItems)
import Receipts.Domain.ReceiptItem (ReceiptItem)

type GetReceipt =
  Capture "qr" String :> Get '[JSON] (Maybe ReceiptResp)

newtype ReceiptResp = ReceiptResp
 { items :: [ReceiptItemResp]
 } deriving (Generic, ToJSON)

data ReceiptItemResp = ReceiptItemResp
  { index :: Int
  , name :: Text
  , quantity :: Double
  , price :: Integer
  } deriving (Generic, ToJSON)

getReceipt :: Impl.Dependencies m => ServerT GetReceipt m
getReceipt qr =
  fmap toResponse <$> Impl.getReceipt qr

toResponse :: Receipt -> ReceiptResp
toResponse
  = ReceiptResp
  . map itemToResp
  . toListOf receiptItems
  where
    itemToResp :: (Int, ReceiptItem) -> ReceiptItemResp
    itemToResp (index, item) = ReceiptItemResp
      index
      (item ^. #name)
      (item ^. #quantity % #value)
      (item ^. #price % #value)
