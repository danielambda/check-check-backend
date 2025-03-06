{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WebAPI.AppM (AppM(..), Env(..)) where

import Servant (Handler, ServerError)

import qualified Database.PostgreSQL.Simple as PG (Connection)
import qualified Database.Redis as Redis (Connection)
import Data.Pool (Pool, withResource)
import Data.UUID.V4 (nextRandom)
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
import Infrastructure.Users.Requests.PGRepository (runRequestsRepositoryT)

newtype AppM a = AppM { runAppM :: ReaderT Env Handler a }
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadError ServerError)

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

instance MonadUUID AppM where
  newUUID = liftIO nextRandom
instance MonadUTCTime AppM where
  currentTime = liftIO getCurrentTime

instance ReceiptsFetching AppM where
  fetchReceiptItems = runReceiptsFetchingT . fetchReceiptItems
instance ReceiptsRepository AppM where
  getReceiptFromRepo = runReceiptsRepositoryT . getReceiptFromRepo
  addReceiptToRepo = (runReceiptsRepositoryT .) . addReceiptToRepo

instance UsersRepository AppM where
  addUserToRepo = runUsersRepositoryT . addUserToRepo
  getUserFromRepo = runUsersRepositoryT . getUserFromRepo
  getSomeUserFromRepo = runUsersRepositoryT . getSomeUserFromRepo
  userExistsInRepo = runUsersRepositoryT . userExistsInRepo
  updateSomeUserInRepo = runUsersRepositoryT . updateSomeUserInRepo

instance RequestsRepository AppM where
  addRequestToRepo = runRequestsRepositoryT . addRequestToRepo
  getIncomingRequestsFromRepo = runRequestsRepositoryT . getIncomingRequestsFromRepo
  getRequestFromRepo = runRequestsRepositoryT . getRequestFromRepo
  markRequestCompletedInRepo = runRequestsRepositoryT . markRequestCompletedInRepo
