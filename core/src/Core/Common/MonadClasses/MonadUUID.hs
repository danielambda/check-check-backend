module Core.Common.MonadClasses.MonadUUID (MonadUUID(..)) where

import Data.UUID (UUID)

class Monad m => MonadUUID m where
  newUUID :: m UUID
