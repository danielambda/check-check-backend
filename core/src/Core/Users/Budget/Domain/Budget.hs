{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE KindSignatures #-}

module Core.Users.Budget.Domain.Budget
  ( Budget(..)
  , addMoney, spendMoney
  ) where

import Optics
  ( (^.), (%~), (&), (<&>)
  , LabelOptic(labelOptic)
  , A_Getter, to, (^?)
  , An_AffineTraversal, atraversalVL, A_Lens, lensVL,
  )

import SmartPrimitives.Positive (Positive)
import Core.Common.Domain.RubKopecks (RubKopecks)

data Budget = Budget
  { amount :: RubKopecks
  , mLowerBound :: Maybe RubKopecks
  }

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

instance (k ~ A_Getter, a ~ Bool, a ~ b)
      => LabelOptic "lowerBoundExceeded" k Budget Budget a b where
  labelOptic = to $ \budget ->
    budget ^? #mLowerBound & maybe False (> budget ^. #amount)

addMoney :: Positive RubKopecks -> Budget -> Budget
addMoney money budget = budget & #amount %~ (+ money ^. #value)

spendMoney :: Positive RubKopecks -> Budget -> (Budget, Bool)
spendMoney money budget =
  let budget' = budget & #amount %~ subtract (money ^. #value)
  in (budget', budget' ^. #lowerBoundExceeded)
