module Core.Users.Domain.UserId (UserId) where

import Data.UUID (UUID)

newtype UserId = UserId UUID
  deriving (Eq)
