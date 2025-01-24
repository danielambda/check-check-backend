{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WebAPI.AppM (AppM(..), Env(..)) where

import Servant (Handler)

import Database.PostgreSQL.Simple (Connection)
import Data.Pool (Pool, withResource)

import Control.Monad.Reader (ReaderT, asks, MonadReader)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Infrastructure.Common.Persistence (MonadConnReader, askConn)
import Infrastructure.Receipts.Fetching (ReceiptsFetchingT(runReceiptsFetchingT))
import Infrastructure.Receipts.PGRepository (ReceiptsRepositoryT(runReceiptsRepositoryT))
import Core.Receipts.MonadClasses.Fetching (ReceiptsFetching (fetchReceiptItems))
import Core.Receipts.MonadClasses.Repository (ReceiptsRepository (getReceiptFromRepo, addReceiptToRepo))

newtype AppM a = AppM { runAppM :: ReaderT Env Handler a }
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO)

data Env = Env
  { envConnPool :: Pool Connection
  , envPort :: Int
  }

instance MonadConnReader AppM where
  askConn = do
    pool <- asks envConnPool
    liftIO $ withResource pool return

instance ReceiptsFetching AppM where
  fetchReceiptItems = runReceiptsFetchingT . fetchReceiptItems

instance ReceiptsRepository AppM where
  getReceiptFromRepo = runReceiptsRepositoryT . getReceiptFromRepo
  addReceiptToRepo = (runReceiptsRepositoryT .) . addReceiptToRepo

