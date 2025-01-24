module Core.Groups.MonadClasses.GoodsRepository (GroupsGoodsRepository(..)) where

import Data.Text (Text)

import SmartPrimitives.NonNegative (NonNegative)
import SmartPrimitives.Positive (Positive)
import Core.Groups.Domain.GroupId (GroupId)

class Monad m => GroupsGoodsRepository m where
  addGoodsToGroupInRepo :: GroupId -> [(Text, Positive Double, NonNegative Integer)] -> m ()
