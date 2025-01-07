{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Receipts
  ( ReceiptsAPI, receiptsServer, ReceiptsEnv(..)
  , MonadReceiptsEnvReader, askReceiptsEnv, initReceiptsEnv
  ) where

import Receipts.API
import qualified Receipts.Fetching as Fetching
import Receipts.Fetching (mkEnv)

newtype ReceiptsEnv = ReceiptsEnv
  { receiptsFetchingEnv :: Fetching.Env
  }

class (Monad m) => MonadReceiptsEnvReader m where
  askReceiptsEnv :: m ReceiptsEnv
instance (Monad m, MonadReceiptsEnvReader m) => Fetching.MonadEnvReader m where
  askEnv = receiptsFetchingEnv <$> askReceiptsEnv

initReceiptsEnv :: IO ReceiptsEnv
initReceiptsEnv = ReceiptsEnv <$> mkEnv

