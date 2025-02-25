{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Core.Users.Budget.Domain.Budget
  ( Budget(..), BudgetLowerBoundStatus(..)
  , addMoney, spendMoney
  ) where

import Optics
  ( (^.), (%~), (&), (<&>)
  , LabelOptic(labelOptic)
  , A_Getter, to, (^?)
  , An_AffineTraversal, atraversalVL, A_Lens, lensVL,
  )

import SmartPrimitives.Positive (Positive)

data Budget = Budget
  { amount :: Integer
  , mLowerBound :: Maybe Integer
  }

data BudgetLowerBoundStatus
  = BudgetLowerBoundExceeded
  | BudgetLowerBoundNotExceeded

instance (k ~ A_Lens, a ~ Integer, a ~ b)
      => LabelOptic "amount" k Budget Budget a b where
  labelOptic = lensVL $ \f Budget{amount, ..} ->
    f amount <&> \amount' -> Budget{amount = amount', ..}

instance (k ~ An_AffineTraversal, a ~ Integer, a ~ b)
      => LabelOptic "mLowerBound" k Budget Budget a b where
  labelOptic = atraversalVL $ \point f Budget{mLowerBound, ..} -> case mLowerBound of
    Just lowerBound -> f lowerBound <&> \lowerBound' -> Budget{mLowerBound = Just lowerBound', ..}
    Nothing -> point Budget{..}

instance (k ~ A_Getter, a ~ BudgetLowerBoundStatus, a ~ b)
      => LabelOptic "lowerBoundStatus" k Budget Budget a b where
  labelOptic = to $ \budget -> case budget ^? #mLowerBound of
    Just lowerBound | budget ^. #amount < lowerBound -> BudgetLowerBoundExceeded
    _ -> BudgetLowerBoundNotExceeded

addMoney :: Positive Integer -> Budget -> Budget
addMoney money budget = budget & #amount %~ (+ money ^. #value)

spendMoney :: Positive Integer -> Budget -> (Budget, BudgetLowerBoundStatus)
spendMoney money budget =
  let budget' = budget & #amount %~ subtract (money ^. #value)
  in (budget', budget' ^. #lowerBoundStatus)
