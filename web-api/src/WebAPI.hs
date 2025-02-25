{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module WebAPI (application) where

import Servant
  (Handler, Application, Proxy(Proxy), ServerT, serve, hoistServer, (:<|>)((:<|>)), (:>))
import Control.Monad.Reader (runReaderT)

import WebAPI.Receipts (ReceiptsAPI, receiptsServer)
import WebAPI.Users (UsersAPI, usersServer)
import WebAPI.AppM (AppM(runAppM), Env)

type API
  =    "receipts" :> ReceiptsAPI
  :<|> "users" :> UsersAPI

server :: ServerT API AppM
server
  =    receiptsServer
  :<|> usersServer

application :: Env -> Application
application env = serve api $ hoistServer api nt server
  where
    api = Proxy :: Proxy API

    nt :: AppM a -> Handler a
    nt = flip runReaderT env . runAppM

