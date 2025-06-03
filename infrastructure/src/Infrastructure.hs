{-# LANGUAGE DerivingVia #-}

module Infrastructure (InfrastructureT(..)) where

import Control.Monad.IO.Class (MonadIO)
import Data.Kind (Type)

import Infrastructure.Common.Persistence (MonadPG)

type InfrastructureT :: (Type -> Type) -> Type -> Type
newtype InfrastructureT m a = InfrastructureT
  { runInfrastructureT :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadPG)

