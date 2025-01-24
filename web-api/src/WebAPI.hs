{-# LANGUAGE DataKinds #-}

module WebAPI (application) where

import Servant (Handler, Application, Proxy (Proxy), ServerT, serve, hoistServer)
import Control.Monad.Reader (runReaderT)

import WebAPI.Receipts (ReceiptsAPI, receiptsServer)
import WebAPI.AppM (AppM(runAppM), Env)

type API = ReceiptsAPI

server :: ServerT API AppM
server = receiptsServer

application :: Env -> Application
application env = serve api $ hoistServer api nt server
  where
    api = Proxy :: Proxy API

    nt :: AppM a -> Handler a
    nt = flip runReaderT env . runAppM

