{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Core.Users.Domain.Budget
  ( Budget(..), BudgetLowerBoundStatus(..)
  , addMoney, spendMoney
  ) where
import SmartPrimitives.Positive (Positive)

import Optics
  ( (^.), (%~), (&), (<&>)
  , makeFieldLabelsFor, LabelOptic(labelOptic)
  , A_Getter, to, (^?)
  , An_AffineTraversal, atraversalVL,
  )

data Budget = Budget
  { amount :: Integer
  , mLowerBound :: Maybe Integer
  }

makeFieldLabelsFor [("amount", "amount")] ''Budget

data BudgetLowerBoundStatus
  = BudgetLowerBoundExceeded
  | BudgetLowerBoundNotExceeded

instance (k ~ An_AffineTraversal, a ~ Integer) => LabelOptic "mLowerBound" k Budget Budget a a where
  labelOptic = atraversalVL $ \point f Budget{mLowerBound, ..} -> case mLowerBound of
    Just lowerBound -> f lowerBound <&> \lowerBound' -> Budget{mLowerBound = Just lowerBound', ..}
    Nothing -> point Budget{..}

instance (k ~ A_Getter, a ~ BudgetLowerBoundStatus) => LabelOptic "lowerBoundStatus" k Budget Budget a a where
  labelOptic = to $ \budget -> case budget ^? #mLowerBound of
    Just lowerBound | budget ^. #amount < lowerBound -> BudgetLowerBoundExceeded
    _ -> BudgetLowerBoundNotExceeded

addMoney :: Positive Integer -> Budget -> Budget
addMoney money budget = budget & #amount %~ (+ money ^. #value)

spendMoney :: Positive Integer -> Budget -> (Budget, BudgetLowerBoundStatus)
spendMoney money budget =
  let budget' = budget & #amount %~ subtract (money ^. #value)
  in (budget', budget' ^. #lowerBoundStatus)
