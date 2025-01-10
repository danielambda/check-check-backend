module Users.Types (User, UserId) where

import Data.Text (Text)
import Data.UUID (UUID)

import Groups.Types (Group)

data User = User
  { id :: UserId
  , username :: Text
  , group :: Group
  }

newtype UserId = UserId UUID
