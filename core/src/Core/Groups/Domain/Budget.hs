{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Core.Groups.Domain.Budget
  ( Budget, emptyBudgetWithLowerBound, mkBudget
  , addMoney, spendMoney
  , BudgetLowerBoundStatus(..)
  ) where
import SmartPrimitives.Positive (Positive)

import Optics
  ( (^.), (%~), sets, Setter, (.~)
  , makeFieldLabelsWith, noPrefixFieldLabels, generateUpdateableOptics, (&), LabelOptic (labelOptic), A_Getter, to,
  )

data Budget = Budget
  { amount :: Integer
  , mLowerBound :: Maybe Integer
  }

makeFieldLabelsWith (noPrefixFieldLabels & generateUpdateableOptics .~ False) ''Budget

data BudgetLowerBoundStatus
  = NoBudgetLowerBound
  | BudgetLowerBoundExeeded
  | BudgetLowerBoundNotExeeded

instance LabelOptic "lowerBoundStatus" A_Getter Budget Budget BudgetLowerBoundStatus BudgetLowerBoundStatus where
  labelOptic = to $ \budget -> case budget ^. #mLowerBound of
    Nothing -> NoBudgetLowerBound
    Just lowerBound | budget ^. #amount < lowerBound ->
      BudgetLowerBoundExeeded
    Just _ | otherwise -> BudgetLowerBoundNotExeeded

amountSetter :: Setter Budget Budget Integer Integer
amountSetter = sets $ \f budget -> budget{amount = f $ amount budget}

mkBudget :: Integer -> Maybe Integer -> Budget
mkBudget = Budget

emptyBudgetWithLowerBound :: Maybe Integer -> Budget
emptyBudgetWithLowerBound = Budget 0

addMoney :: Positive Integer -> Budget -> Budget
addMoney money budget = budget & amountSetter %~ (+ money ^. #value)

spendMoney :: Positive Integer -> Budget -> (Budget, BudgetLowerBoundStatus)
spendMoney money budget =
  let budget' = budget & amountSetter %~ subtract (money ^. #value)
  in (budget', budget' ^. #lowerBoundStatus)
