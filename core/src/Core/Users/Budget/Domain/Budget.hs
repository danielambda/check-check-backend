{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Core.Users.Budget.Domain.Budget
  ( Budget(..), BudgetLowerBoundStatus(..)
  , RoundingData'(..), RoundingData, RoundingStrategy(..)
  , applyDelta
  , spend, spendRounded
  , receive, receiveRounded
  ) where

import Optics
  ( (^.), (%~), (&), (<&>)
  , LabelOptic(labelOptic)
  , A_Getter, to, (^?)
  , An_AffineTraversal, atraversalVL, A_Lens, lensVL,
  )

import Core.Common.Domain.RubKopecks (RubKopecks)
import SmartPrimitives.Positive (pattern Positive, Positive, unPositive)
import Prelude hiding (round)

data Budget = Budget
  { amount :: RubKopecks
  , mLowerBound :: Maybe RubKopecks
  }

data BudgetLowerBoundStatus
  = BudgetLowerBoundExceeded
  | BudgetLowerBoundNotExceeded
  deriving Eq

type RoundingData = RoundingData' RubKopecks
data RoundingData' a = RoundingData
  { eps :: Positive a
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

applyDelta :: RubKopecks -> Budget -> Budget
applyDelta delta budget = budget & #amount %~ (+ delta)

spend :: Positive RubKopecks -> Budget -> Budget
spend
  = applyDelta
  . negate
  . unPositive

spendRounded :: RoundingData -> Positive RubKopecks -> Budget -> Budget
spendRounded roundingData
  = applyDelta
  . negate
  . roundWith roundingData
  . unPositive

receive :: Positive RubKopecks -> Budget -> Budget
receive
  = applyDelta
  . unPositive

receiveRounded :: RoundingData -> Positive RubKopecks -> Budget -> Budget
receiveRounded roundingData
  = applyDelta
  . roundWith roundingData
  . unPositive

roundWith :: Integral a => RoundingData' a -> a -> a
roundWith RoundingData{ eps, strategy } = case strategy of
  RoundUp -> roundUp eps
  RoundToNearest -> roundToNearest eps
  RoundDown -> roundDown eps

roundUp :: Integral a => Positive a -> a -> a
roundUp (Positive eps) x = ((x + eps - 1) `div` eps) * eps

roundToNearest :: Integral a => Positive a -> a -> a
roundToNearest (Positive eps) x = (x + eps `div` 2) `div` eps * eps

roundDown :: Integral a => Positive a -> a -> a
roundDown (Positive eps) x = (x `div` eps) * eps
