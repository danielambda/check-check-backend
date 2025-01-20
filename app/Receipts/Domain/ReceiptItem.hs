{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}

module Receipts.Domain.ReceiptItem (ReceiptItem, mkReceiptItem, tryMerge) where

import Data.Text (Text)
import Optics (LabelOptic(labelOptic), A_Getter, (^.), (%~), sets, Setter)
import Optics.Getter (to)

import Shared.Types.Positive (Positive, plus)
import Data.Function ((&))

data ReceiptItem = ReceiptItem
  { name :: Text
  , price :: Positive Integer
  , quantity :: Positive Double
  }

instance LabelOptic "name" A_Getter ReceiptItem ReceiptItem Text Text where
  labelOptic = to name
instance LabelOptic "price" A_Getter ReceiptItem ReceiptItem (Positive Integer) (Positive Integer) where
  labelOptic = to price
instance LabelOptic "quantity" A_Getter ReceiptItem ReceiptItem (Positive Double) (Positive Double) where
  labelOptic = to quantity

mkReceiptItem :: Text -> Positive Integer -> Positive Double -> ReceiptItem
mkReceiptItem = ReceiptItem

tryMerge :: ReceiptItem -> ReceiptItem -> Maybe ReceiptItem
tryMerge item1 item2 =
  if item1 ^. #name == item2 ^. #name then
    Just $ item1 & quantitySetter %~ plus (item2 ^. #quantity)
  else
    Nothing

quantitySetter :: Setter ReceiptItem ReceiptItem (Positive Double) (Positive Double)
quantitySetter = sets $ \f item -> item{quantity = f $ quantity item}

