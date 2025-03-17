{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Telegram.Bot.AppM
  ( AppM(..), AppError(..), AppEnv(..)
  , tg
  ) where

import Data.Time (UTCTime)
import Servant.Auth.Client (Token)
import Servant.Client (ClientError, ClientEnv)
import Telegram.Bot.Simple (BotM)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, MonadReader)
import Control.Monad.State (StateT, MonadState)
import Control.Monad.Trans (lift)

import ClientMUtils (FromClientError (..), HasKeyedClientEnv (..))

newtype AppM a = AppM
  { unAppM :: ReaderT AppEnv (StateT (Maybe (Token, UTCTime)) (ExceptT AppError BotM)) a }
  deriving
    ( Functor, Applicative
    , Monad, MonadIO
    , MonadReader AppEnv
    , MonadState (Maybe (Token, UTCTime))
    , MonadError AppError
    )

newtype AppError = AppClientError ClientError
  deriving (Show)

instance FromClientError AppError where
  fromClientError = AppClientError

data AppEnv = AppEnv
  { backendClientEnv :: ClientEnv
  , authClientEnv :: ClientEnv
  , secret :: String
  }

instance HasKeyedClientEnv AppEnv "backend" where
  getClientEnv _ = backendClientEnv

instance HasKeyedClientEnv AppEnv "auth" where
  getClientEnv _ = authClientEnv

tg :: BotM a -> AppM a
tg = AppM . lift . lift . lift

