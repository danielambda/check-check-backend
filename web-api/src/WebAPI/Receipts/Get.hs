{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module WebAPI.Receipts.Get
  ( Dependencies
  , GetReceipt, getReceipt
  , ReceiptResp, ReceiptItemResp
  ) where

import Servant (ServerT, Capture, (:>), JSON, Get, err404, ServerError)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import Optics ((^.), toListOf, (<&>), (&))

import GHC.Generics (Generic)
import Control.Monad.Error.Class (throwError, MonadError)

import Core.Common.Operators ((^^.))
import Core.Receipts.Domain.Receipt (Receipt, receiptItems)
import Core.Receipts.Domain.ReceiptItem (ReceiptItem)
import qualified Core.Receipts.Get as Impl (get, Dependencies)
import SmartPrimitives.Positive (Positive)

type GetReceipt =
  Capture "qr" Text :> Get '[JSON] ReceiptResp

newtype ReceiptResp = ReceiptResp
 { items :: [ReceiptItemResp]
 } deriving (Generic, ToJSON)

data ReceiptItemResp = ReceiptItemResp
  { index :: Int
  , name :: Text
  , quantity :: Positive Double
  , price :: Positive Integer
  } deriving (Generic, ToJSON)

type Dependencies m = (Impl.Dependencies m, MonadError ServerError m)
getReceipt :: Dependencies m => ServerT GetReceipt m
getReceipt qr = qr
   &  Impl.get
  <&> fmap toResp
  >>= maybe (throwError err404) return

toResp :: Receipt -> ReceiptResp
toResp
  = ReceiptResp
  . map itemToResp
  . toListOf receiptItems
  where
    itemToResp :: (Int, ReceiptItem) -> ReceiptItemResp
    itemToResp (index, item) = ReceiptItemResp
      index
      (item  ^. #name)
      (item  ^. #quantity)
      (item ^^. #price)
