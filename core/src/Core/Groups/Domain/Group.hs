module Core.Groups.Domain.Group (Group, mkGroup) where

import Core.Users.Domain.UserId (UserId)
import Core.Groups.Domain.Primitives (GroupName)

data Group = Group
  { name :: GroupName
  , budget :: Integer
  , members :: [UserId]
  , budgetLowerBound :: Maybe Integer
  }

-- TODO take budget and budgetLowerBound to separate type

mkGroup :: GroupName -> Integer -> [UserId] -> Maybe Integer -> Group
mkGroup = Group
