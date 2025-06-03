{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

module WebAPI.AppM (AppM(..), Env(..)) where

import Servant (Handler, ServerError)

import qualified Database.PostgreSQL.Simple as PG (Connection)
import qualified Data.UUID.V4 as V4 (nextRandom)
import Data.Pool (Pool, withResource)
import Data.Time (getCurrentTime)

import Control.Monad.Reader (ReaderT, asks, MonadReader)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Error.Class (MonadError)
import GHC.MVar (MVar)

import Infrastructure.Common.Persistence (MonadPG(..))
import Core.Common.MonadClasses.MonadUUID (MonadUUID(..))
import Core.Common.MonadClasses.MonadUTCTime (MonadUTCTime(..))

newtype AppM a = AppM { runAppM :: ReaderT Env Handler a }
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadError ServerError)

data Env = Env
  { pgConnPool :: Pool PG.Connection
  , budgetAmountMVar :: MVar Integer
  }

instance MonadUUID AppM where
  newUUID = liftIO V4.nextRandom

instance MonadUTCTime AppM where
  currentTime = liftIO getCurrentTime

instance MonadPG AppM where
  askConn = do
    pool <- asks pgConnPool
    liftIO $ withResource pool return

