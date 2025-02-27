{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}

module Core.Users.Budget.Domain.Budget
  ( Budget(..), BudgetLowerBoundStatus(..)
  , RoundingData(..), RoundingStrategy(..)
  , applyDelta, applyRoundedDelta
  ) where

import Optics
  ( (^.), (%~), (&), (<&>)
  , LabelOptic(labelOptic)
  , A_Getter, to, (^?)
  , An_AffineTraversal, atraversalVL, A_Lens, lensVL,
  )

import Core.Common.Domain.RubKopecks (RubKopecks)
import SmartPrimitives.Positive (pattern Positive, Positive)
import Prelude hiding (round)

data Budget = Budget
  { amount :: RubKopecks
  , mLowerBound :: Maybe RubKopecks
  }

data BudgetLowerBoundStatus
  = BudgetLowerBoundExceeded
  | BudgetLowerBoundNotExceeded
  deriving Eq

data RoundingData = RoundingData
  { eps :: Positive RubKopecks
  , strategy :: RoundingStrategy
  }

data RoundingStrategy
  = RoundUp
  | RoundToNearest
  | RoundDown

instance (k ~ A_Lens, a ~ RubKopecks, a ~ b)
      => LabelOptic "amount" k Budget Budget a b where
  labelOptic = lensVL $ \f Budget{amount, ..} ->
    f amount <&> \amount' -> Budget{amount = amount', ..}

instance (k ~ An_AffineTraversal, a ~ RubKopecks, a ~ b)
      => LabelOptic "mLowerBound" k Budget Budget a b where
  labelOptic = atraversalVL $ \point f Budget{mLowerBound, ..} -> case mLowerBound of
    Just lowerBound -> f lowerBound <&> \lowerBound' ->
      Budget{mLowerBound = Just lowerBound', ..}
    Nothing -> point Budget{..}

instance (k ~ A_Getter, a ~ BudgetLowerBoundStatus, a ~ b)
      => LabelOptic "lowerBoundStatus" k Budget Budget a b where
  labelOptic = to $ \budget -> case budget ^? #mLowerBound of
    Just lowerBound | budget ^. #amount < lowerBound -> BudgetLowerBoundExceeded
    _ -> BudgetLowerBoundNotExceeded

applyDelta :: RubKopecks -> Budget -> (Budget, BudgetLowerBoundStatus)
applyDelta delta budget =
  let budget' = budget & #amount %~ (+ delta)
  in (budget', budget' ^. #lowerBoundStatus)

applyRoundedDelta :: RoundingData -> RubKopecks -> Budget -> (Budget, BudgetLowerBoundStatus)
applyRoundedDelta RoundingData{ eps, strategy } = applyDelta . round
  where
    round = case strategy of
      RoundUp -> roundUp eps
      RoundToNearest -> roundToNearest eps
      RoundDown -> roundDown eps

roundUp :: Integral a => Positive a -> a -> a
roundUp (Positive eps) x = ((x + eps - 1) `div` eps) * eps

roundToNearest :: Integral a => Positive a -> a -> a
roundToNearest (Positive eps) x = (x + eps `div` 2) `div` eps * eps

roundDown :: Integral a => Positive a -> a -> a
roundDown (Positive eps) x = (x `div` eps) * eps
