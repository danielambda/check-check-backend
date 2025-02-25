{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Core.Common.Domain.RubKopecks (RubKopecks(..), positiveRubKopecks) where

import Optics (LabelOptic (labelOptic), A_Getter, to)
import SmartPrimitives.Positive (Positive, unPositive, mkUnsafePositive)

newtype RubKopecks = RubKopecks { value :: Integer }
  deriving (Eq, Ord, Num)

positiveRubKopecks :: Positive Integer -> Positive RubKopecks
positiveRubKopecks = mkUnsafePositive . RubKopecks . unPositive

instance (a ~ Integer, a ~ b) => LabelOptic "value" A_Getter RubKopecks RubKopecks a b where
  labelOptic = to value

instance (a ~ Positive Integer, a ~ b)
      => LabelOptic "posValue" A_Getter (Positive RubKopecks) (Positive RubKopecks) a b where
  labelOptic = to $ mkUnsafePositive . value . unPositive
