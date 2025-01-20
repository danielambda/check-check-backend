{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}

module Receipts.Domain.Receipt (Receipt, mkReceipt, receiptItems) where

import Optics (view, ifolding, IxFold)

import Data.List.NonEmpty (NonEmpty, nonEmpty, toList)
import GHC.Generics (Generic)

import Receipts.Domain.ReceiptItem (ReceiptItem, tryMerge)
import Data.List (sortOn)

newtype Receipt = Receipt
  { items :: NonEmpty ReceiptItem
  } deriving (Generic)

receiptItems :: IxFold Int Receipt ReceiptItem
receiptItems = ifolding (toList . items)

mkReceipt :: [ReceiptItem] -> Maybe Receipt
mkReceipt
  = fmap Receipt
  . nonEmpty
  . mergeItems

mergeItems :: [ReceiptItem] -> [ReceiptItem]
mergeItems
  = foldr merge []
  . sortOn (view #name)
  where
    merge item [] = [item]
    merge item (headItem:rest) =
      case tryMerge item headItem of
        Nothing -> item:headItem:rest
        Just mergedItem -> mergedItem:rest

