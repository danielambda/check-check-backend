{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module App (Env(..), application) where

import Servant ((:<|>)((:<|>)), Handler, Application, Proxy (Proxy), ServerT, serve, hoistServer, JSON, Get, (:>))
import Database.PostgreSQL.Simple (Connection)
import Control.Monad.Reader (ReaderT (runReaderT), asks, MonadReader)

import Control.Monad.IO.Class (MonadIO)

import Common.Persistence (MonadConnPoolReader, askConnPool)
import Receipts
  ( ReceiptsEnv, MonadReceiptsEnvReader, askReceiptsEnv
  , ReceiptsAPI, receiptsServer
  )
import Groups (GroupsAPI, groupsServer)
import Groups.API (groupsServer)

newtype AppM a = AppM { runAppM :: ReaderT Env Handler a }
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO)

data Env = Env
  { envConnPool :: Connection
  , envPort :: Int
  , envReceiptsEnv :: ReceiptsEnv
  }

instance MonadReceiptsEnvReader AppM where
  askReceiptsEnv = asks envReceiptsEnv
instance MonadConnPoolReader AppM where
  askConnPool = asks envConnPool

type API
  =    ReceiptsAPI
  :<|> GroupsAPI

server :: ServerT API AppM
server
  =    receiptsServer
  :<|> groupsServer

application :: Env -> Application
application env = serve api $ hoistServer api nt server
  where
    api = Proxy :: Proxy API

    nt :: AppM a -> Handler a
    nt = flip runReaderT env . runAppM

