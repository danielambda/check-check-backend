{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module WebAPI (mkApp) where

import Servant
  (Handler, Application, Proxy(Proxy), ServerT, (:<|>)((:<|>)), (:>), serveWithContextT)
import Control.Monad.Reader (runReaderT)

import WebAPI.Receipts (ReceiptsAPI, receiptsServer)
import WebAPI.Users (UsersAPI, usersServer)
import WebAPI.AppM (AppM(runAppM), Env)
import WebAPI.Auth (config)

type API
  =    "receipts" :> ReceiptsAPI
  :<|> "users" :> UsersAPI

server :: ServerT API AppM
server
  =    receiptsServer
  :<|> usersServer

mkApp :: Env -> IO Application
mkApp env = do
  -- serve api $ hoistServer api nt server
  ctx <- config
  return $ serveWithContextT api ctx nt server
  where
    api = Proxy :: Proxy API

    nt :: AppM a -> Handler a
    nt = flip runReaderT env . runAppM

