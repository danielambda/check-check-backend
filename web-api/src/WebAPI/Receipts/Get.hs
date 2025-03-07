{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module WebAPI.Receipts.Get
  ( Dependencies
  , GetReceipt, getReceipt
  , ReceiptResp, ReceiptItemResp
  ) where

import Servant (ServerT, err404, ServerError)
import Optics ((^.), toListOf, (<&>), (&), (%))

import Control.Monad.Error.Class (throwError, MonadError)

import CheckCheck.Contracts.Receipts (GetReceipt, ReceiptResp (ReceiptResp), ReceiptItemResp (..))
import Core.Receipts.Domain.Receipt (Receipt, receiptItems)
import Core.Receipts.Domain.ReceiptItem (ReceiptItem)
import qualified Core.Receipts.Get as Impl (get, Dependencies)

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
      { index
      , name = item  ^. #name
      , quantity = item  ^. #quantity
      , price = item ^. #price % #posValue
      }
