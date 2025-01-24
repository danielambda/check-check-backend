{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BlockArguments #-}

module Core.Groups.AddGoodsToGroup
  ( AddGoodsToGroupData(..)
  , AddGoodsToGroupGoodsData(..)
  , Dependencies
  , addGoodsToGroup
  ) where

import Data.Text (Text)

import SmartPrimitives.Positive (Positive, mkPositive, mult, sumPositive, ceilingPositive)
import SmartPrimitives.NonNegative (NonNegative)
import Core.Groups.MonadClasses.Repository
  ( GroupsCommandRepository (addGoodsToGroupInRepo)
  , GroupsQueryRepository (getGroupBudgetFromRepo))
import Core.Groups.Domain.GroupId (GroupId)
import Core.Groups.Domain.Budget (BudgetLowerBoundStatus, spendMoney)
import Optics ((^.))
import Data.Maybe (mapMaybe)
import Data.List.NonEmpty (nonEmpty)

data AddGoodsToGroupData = AddGoodsToGroupData
  { groupId :: GroupId
  , goods :: [AddGoodsToGroupGoodsData]
  }

data AddGoodsToGroupGoodsData = AddGoodsToGroupGoodsData
  { name :: Text
  , quantity :: Positive Double
  , price :: NonNegative Integer
  }

type Dependencies m = (GroupsCommandRepository m, GroupsQueryRepository m)
addGoodsToGroup :: Dependencies m
                => AddGoodsToGroupData -> m (Maybe BudgetLowerBoundStatus)
addGoodsToGroup AddGoodsToGroupData{ groupId, goods } =
  getGroupBudgetFromRepo groupId
  >>= maybe (pure Nothing) storeGoodsAndReturnBudgetLowerBoundStatus
  where
    storeGoodsAndReturnBudgetLowerBoundStatus budget = do
      addGoodsToGroupInRepo groupId (tuppled <$> goods)
      return $ Just lowerBoundStatus
      where
        lowerBoundStatus = case totalCostOf goods of
          Nothing -> budget ^. #lowerBoundStatus
          Just totalCost -> snd $ spendMoney totalCost budget

    tuppled AddGoodsToGroupGoodsData{ name, quantity, price } =
      (name, quantity, price)

    totalCostOf
      = fmap (ceilingPositive . sumPositive)
      . nonEmpty
      . mapMaybe calculateCost
      where
        calculateCost AddGoodsToGroupGoodsData{ quantity, price } =
          let mPrice = mkPositive $ fromInteger $ price ^. #value
          in mult quantity <$> mPrice
