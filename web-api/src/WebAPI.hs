{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module WebAPI (Env(..), application) where

import Servant ((:<|>)((:<|>)), Handler, Application, Proxy (Proxy), ServerT, serve, hoistServer)
import Database.PostgreSQL.Simple (Connection)
import Control.Monad.Reader (ReaderT (runReaderT), asks, MonadReader)

import Control.Monad.IO.Class (MonadIO)

import Shared.Persistence (MonadConnPoolReader, askConnPool)
import WebAPI.Receipts (ReceiptsAPI, receiptsServer)

newtype AppM a = AppM { runAppM :: ReaderT Env Handler a }
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO)

data Env = Env
  { envConnPool :: Connection
  , envPort :: Int
  }

type API = ReceiptsAPI

server :: ServerT API AppM
server = receiptsServer

application :: Env -> Application
application env = serve api $ hoistServer api nt server
  where
    api = Proxy :: Proxy API

    nt :: AppM a -> Handler a
    nt = flip runReaderT env . runAppM

instance MonadConnPoolReader AppM where
  askConnPool = asks envConnPool
