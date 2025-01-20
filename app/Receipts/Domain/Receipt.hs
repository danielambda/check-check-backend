{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}

module Receipts.Domain.Receipt (Receipt, mkReceipt) where

import Optics (LabelOptic(labelOptic), A_Getter, to, view)

import Data.List.NonEmpty (NonEmpty, nonEmpty, toList)
import GHC.Generics (Generic)

import Receipts.Domain.ReceiptItem (ReceiptItem, tryMerge)
import Data.List (sortOn)

newtype Receipt = Receipt
  { items :: NonEmpty ReceiptItem
  } deriving (Generic)

instance LabelOptic "items" A_Getter Receipt Receipt [ReceiptItem] [ReceiptItem] where
  labelOptic = to $ toList . items

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

