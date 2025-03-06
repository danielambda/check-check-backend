{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module WebAPI (mkApp) where

import Servant.Auth.Server (defaultCookieSettings)
import Servant
  ( Handler, Application, Proxy(..)
  , ServerT
  , (:<|>)((:<|>)), (:>)
  , serveWithContextT
  , Context (..)
  )
import Control.Monad.Reader (runReaderT)

import WebAPI.Receipts (ReceiptsAPI, receiptsServer)
import WebAPI.Users (UsersAPI, usersServer)
import WebAPI.AppM (AppM(runAppM), Env)
import WebAPI.Auth (getJwtSettings)

type API
  =    "receipts" :> ReceiptsAPI
  :<|> UsersAPI

server :: ServerT API AppM
server
  =    receiptsServer
  :<|> usersServer

mkApp :: Env -> IO Application
mkApp env = do
  jwtSettings <- getJwtSettings
  let ctx = jwtSettings :. defaultCookieSettings :. EmptyContext
  return $ serveWithContextT api ctx nt server
  where
    api = Proxy :: Proxy API

    nt :: AppM a -> Handler a
    nt = flip runReaderT env . runAppM

