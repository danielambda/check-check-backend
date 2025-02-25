{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell, DataKinds
  , FlexibleInstances, TypeFamilies
  , UndecidableInstances, TypeOperators
  #-}

module Core.Receipts.Domain.ReceiptItem (ReceiptItem(..), tryMerge) where

import Data.Text (Text)
import Optics
  ( (^.), (%~), (&), sets, Setter
  , makeFieldLabelsNoPrefix
  )

import SmartPrimitives.Positive (Positive, plus)
import Core.Common.Domain.RubKopecks (RubKopecks)

data ReceiptItem = ReceiptItem
  { name :: Text
  , price :: Positive RubKopecks
  , quantity :: Positive Double
  }

makeFieldLabelsNoPrefix ''ReceiptItem

tryMerge :: ReceiptItem -> ReceiptItem -> Maybe ReceiptItem
tryMerge item1 item2 =
  if item1 ^. #name == item2 ^. #name then
    Just $ item1 & quantitySetter %~ plus (item2 ^. #quantity)
  else
    Nothing

quantitySetter :: Setter ReceiptItem ReceiptItem (Positive Double) (Positive Double)
quantitySetter = sets $ \f item -> item{quantity = f $ quantity item}

