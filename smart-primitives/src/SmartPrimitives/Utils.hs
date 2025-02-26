module SmartPrimitives.Utils
  ( posZeroNeg, PosZeroNeg(..)
  ) where

import SmartPrimitives.Positive (Positive, mkPositive)
import SmartPrimitives.Negative (Negative, mkNegative)

data PosZeroNeg a
  = Pos (Positive a)
  | Zero
  | Neg (Negative a)

posZeroNeg :: (Num a, Ord a) => a -> PosZeroNeg a
posZeroNeg a = maybe (maybe Zero Neg $ mkNegative a) Pos $ mkPositive a
