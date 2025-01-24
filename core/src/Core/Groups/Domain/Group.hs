{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Core.Groups.Domain.Group (Group, mkNewGroup) where

import Optics
  (makeFieldLabelsWith, noPrefixFieldLabels, generateUpdateableOptics, (&), (.~))

import Core.Users.Domain.UserId (UserId)
import Core.Groups.Domain.Primitives (GroupName)
import Core.Groups.Domain.Budget (Budget, emptyBudgetWithLowerBound)
import Core.Groups.Domain.GroupId (GroupId, newGroupId)
import Core.Common.MonadClasses.MonadUUID (MonadUUID)

data Group = Group
  { groupId :: GroupId
  , name :: GroupName
  , members :: [UserId]
  , budget :: Budget
  }

makeFieldLabelsWith (noPrefixFieldLabels & generateUpdateableOptics .~ False) ''Group

mkNewGroup :: MonadUUID m => GroupName -> [UserId] -> Maybe Integer -> m Group
mkNewGroup name members budgetLowerBound = do
  groupId <- newGroupId
  let budget = emptyBudgetWithLowerBound budgetLowerBound
  return $ Group { groupId, name, budget, members }
