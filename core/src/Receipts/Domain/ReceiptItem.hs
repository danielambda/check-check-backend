{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell, DataKinds
  , FlexibleInstances, TypeFamilies
  , UndecidableInstances, TypeOperators
  #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Receipts.Domain.ReceiptItem (ReceiptItem, mkReceiptItem, tryMerge) where

import Data.Text (Text)
import Optics
  ( (^.), (%~), sets, Setter, (.~)
  , makeFieldLabelsWith, noPrefixFieldLabels, generateUpdateableOptics,
  )

import Data.Function ((&))

import SmartPrimitives.Positive (Positive, plus)

data ReceiptItem = ReceiptItem
  { name :: Text
  , price :: Positive Integer
  , quantity :: Positive Double
  }

makeFieldLabelsWith (noPrefixFieldLabels & generateUpdateableOptics .~ False) ''ReceiptItem

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

