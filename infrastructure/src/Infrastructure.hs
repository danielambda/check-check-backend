{-# LANGUAGE DerivingVia #-}

module Infrastructure (InfrastructureT(..)) where

import Control.Monad.IO.Class (MonadIO)
import Data.Kind (Type)

import Core.Receipts.MonadClasses.Fetching (ReceiptsFetching)
import Core.Receipts.MonadClasses.Repository (ReceiptsRepository)
import Core.Users.MonadClasses.Repository (UsersRepository)
import Core.Users.Requests.MonadClasses.Repository (RequestsRepository)
import Infrastructure.Receipts.Fetching (ReceiptsFetchingT(..))
import Infrastructure.Receipts.PGRepository (ReceiptsRepositoryT(..))
import Infrastructure.Users.PGRepository (UsersRepositoryT(..))
import Infrastructure.Users.Requests.PGRepository (RequestsRepositoryT(..))
import Infrastructure.Common.Persistence (MonadPG)

type InfrastructureT :: (Type -> Type) -> Type -> Type
newtype InfrastructureT m a = InfrastructureT
  { runInfrastructureT :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadPG)

deriving via ReceiptsFetchingT (InfrastructureT m) instance (MonadIO m) =>
  ReceiptsFetching (InfrastructureT m)
deriving via ReceiptsRepositoryT (InfrastructureT m) instance (MonadIO m, MonadPG m) =>
  ReceiptsRepository (InfrastructureT m)

deriving via UsersRepositoryT (InfrastructureT m) instance (MonadIO m, MonadPG m) =>
  UsersRepository (InfrastructureT m)

deriving via RequestsRepositoryT (InfrastructureT m) instance (MonadIO m, MonadPG m) =>
  RequestsRepository (InfrastructureT m)

