{-# LANGUAGE DataKinds #-}

module WebAPI (mkApp) where

import Servant
  (Application, Proxy(Proxy), Handler, ServerT, (:>), (:<|>)((:<|>)), serve, hoistServer)

import Control.Monad.Reader (runReaderT)

import WebAPI.AppM (AppM(runAppM), Env)
import WebAPI.Budget (BudgetAPI, budgetServer)
import WebAPI.Requests (RequestsAPI, requestsServer)

type API
  =    "requests" :> RequestsAPI
  :<|> "budget" :> BudgetAPI

server :: ServerT API AppM
server
  =    requestsServer
  :<|> budgetServer

mkApp :: Env -> IO Application
mkApp env = do
  return $ serve api $ hoistServer api nt server
  where
    api = Proxy :: Proxy API

    nt :: AppM a -> Handler a
    nt = flip runReaderT env . runAppM

