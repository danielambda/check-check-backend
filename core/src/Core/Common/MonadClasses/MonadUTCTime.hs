module Core.Common.MonadClasses.MonadUTCTime (MonadUTCTime(..)) where

import Data.Time (UTCTime)

class Monad m => MonadUTCTime m where
  currentTime :: m UTCTime
