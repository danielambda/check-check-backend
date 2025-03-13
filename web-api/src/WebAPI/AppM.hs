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

import Infrastructure.Common.Persistence (MonadConnReader, askConn)
import Infrastructure.Receipts.Fetching (ReceiptsFetchingT(..))
import Infrastructure.Receipts.PGRepository (ReceiptsRepositoryT(..))
import Infrastructure.Users.PGRepository (UsersRepositoryT(..))
import Core.Receipts.MonadClasses.Fetching (ReceiptsFetching (..))
import Core.Receipts.MonadClasses.Repository (ReceiptsRepository (..))
import Core.Common.MonadClasses.MonadUUID (MonadUUID(..))
import Core.Common.MonadClasses.MonadUTCTime (MonadUTCTime(..))
import Core.Users.MonadClasses.Repository (UsersRepository(..))
import Core.Users.Requests.MonadClasses.Repository (RequestsRepository(..))
import Infrastructure.Users.Requests.PGRepository (RequestsRepositoryT(..))

newtype AppM a = AppM { runAppM :: ReaderT Env Handler a }
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadError ServerError)
  deriving ReceiptsFetching via ReceiptsFetchingT AppM
  deriving ReceiptsRepository via ReceiptsRepositoryT AppM
  deriving UsersRepository via UsersRepositoryT AppM
  deriving RequestsRepository via RequestsRepositoryT AppM

instance MonadUUID AppM where newUUID = liftIO V4.nextRandom
instance MonadUTCTime AppM where currentTime = liftIO getCurrentTime

data Env = Env
  { pgConnPool :: Pool PG.Connection
  , redisConnPool :: Pool Redis.Connection
  }

instance MonadConnReader AppM where
  askConn = do
    pool <- asks pgConnPool
    liftIO $ withResource pool return

askRedisConn :: AppM Redis.Connection
askRedisConn = do
  pool <- asks redisConnPool
  liftIO $ withResource pool return

