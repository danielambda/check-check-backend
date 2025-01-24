module Core.Users.Domain.User (User) where

import Data.Text (Text)

import Core.Groups.Domain.GroupId (GroupId)
import Core.Users.Domain.UserId (UserId)

data User = User
  { userId :: UserId
  , username :: Text
  , groupId :: Maybe GroupId
  }

instance Eq User where
  u1 == u2 = userId u1 == userId u2
