{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Core.Groups.AddGoodsToGroup
  ( AddGoodsToGroupData(..)
  , AddGoodsToGroupGoodsData(..)
  , Dependencies
  , addGoodsToGroup
  ) where

import Data.Text (Text)

import SmartPrimitives.Positive (Positive)
import SmartPrimitives.NonNegative (NonNegative)
import Core.Groups.Domain.GroupId (GroupId)
import Core.Groups.MonadClasses.GoodsRepository (GroupsGoodsRepository (addGoodsToGroupInRepo))

data AddGoodsToGroupData = AddGoodsToGroupData
  { groupId :: GroupId
  , goods :: [AddGoodsToGroupGoodsData]
  }

data AddGoodsToGroupGoodsData = AddGoodsToGroupGoodsData
  { name :: Text
  , quantity :: Positive Double
  , price :: NonNegative Integer
  }

type Dependencies m = (GroupsGoodsRepository m)
addGoodsToGroup :: Dependencies m => AddGoodsToGroupData -> m ()
addGoodsToGroup AddGoodsToGroupData{ groupId, goods } = do
  addGoodsToGroupInRepo groupId (tuppled <$> goods)
  where
    tuppled AddGoodsToGroupGoodsData{ name, quantity, price } =
      (name, quantity, price)
