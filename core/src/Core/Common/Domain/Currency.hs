{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Core.Common.Domain.Currency (Currency(..), CurrencyKind(..), SomeCurrency(..)) where

import SmartPrimitives.Positive (Positive)
import Optics (LabelOptic (labelOptic), A_Getter, to)

data Currency (kind :: CurrencyKind) where
  Kopecks :: Positive Integer -> Currency 'Rubbles

data CurrencyKind = Rubbles

data SomeCurrency where
  SomeCurrency :: Currency kind -> SomeCurrency

instance (a ~ Positive Integer, a ~ b) => LabelOptic "value" A_Getter SomeCurrency SomeCurrency a b where
  labelOptic = to $ \(SomeCurrency (Kopecks x)) -> x
