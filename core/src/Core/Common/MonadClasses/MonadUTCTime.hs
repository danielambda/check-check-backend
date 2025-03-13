module Core.Common.MonadClasses.MonadUTCTime (MonadUTCTime(..)) where

import Data.Time (UTCTime, getCurrentTime)

class Monad m => MonadUTCTime m where
  currentTime :: m UTCTime

instance MonadUTCTime IO where
  currentTime = getCurrentTime
