{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

module WebAPI.AppM (AppM(..), Env(..)) where

import Servant (Handler, ServerError)

import qualified Database.PostgreSQL.Simple as PG (Connection)
import qualified Database.Redis as Redis (Connection)
import qualified Data.UUID.V4 as V4 (nextRandom)
import Data.Pool (Pool, withResource)
import Data.Time (getCurrentTime)

import Control.Monad.Reader (ReaderT, asks, MonadReader)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Error.Class (MonadError)

import Infrastructure (InfrastructureT(..))
import Infrastructure.Common.Persistence (MonadPG(..))
import Core.Common.MonadClasses.MonadUUID (MonadUUID(..))
import Core.Common.MonadClasses.MonadUTCTime (MonadUTCTime(..))
import Core.Receipts.MonadClasses.Fetching (ReceiptsFetching)
import Core.Receipts.MonadClasses.Repository (ReceiptsRepository)
import Core.Users.MonadClasses.Repository (UsersRepository)
import Core.Users.Requests.MonadClasses.Repository (RequestsRepository)

newtype AppM a = AppM { runAppM :: ReaderT Env Handler a }
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadError ServerError)
  deriving
    ( ReceiptsFetching, ReceiptsRepository
    , UsersRepository, RequestsRepository
    ) via InfrastructureT AppM

instance MonadUUID AppM where newUUID = liftIO V4.nextRandom
instance MonadUTCTime AppM where currentTime = liftIO getCurrentTime
instance MonadPG AppM where
  askConn = do
    pool <- asks pgConnPool
    liftIO $ withResource pool return

data Env = Env
  { pgConnPool :: Pool PG.Connection
  , redisConnPool :: Pool Redis.Connection
  }

_askRedisConn :: AppM Redis.Connection
_askRedisConn = do
  pool <- asks redisConnPool
  liftIO $ withResource pool return

