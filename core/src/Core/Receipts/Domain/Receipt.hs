{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}

module Core.Receipts.Domain.Receipt (Receipt, mkReceipt, receiptItems) where

import Optics (view, Fold, folding)

import Data.List.NonEmpty (NonEmpty, nonEmpty, toList)
import GHC.Generics (Generic)

import Core.Receipts.Domain.ReceiptItem (ReceiptItem, tryMerge)
import Data.List (sortOn)

newtype Receipt = Receipt
  { items :: NonEmpty ReceiptItem
  } deriving (Generic)

receiptItems :: Fold Receipt (Int, ReceiptItem)
receiptItems = folding $ zip [0..] . toList . items

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

