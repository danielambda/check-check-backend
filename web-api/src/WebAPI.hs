{-# LANGUAGE DataKinds #-}

module WebAPI (mkApp) where

import Servant.Auth.Server (defaultCookieSettings)
import Servant
  ( Handler, Application, Proxy(..)
  , ServerT
  , (:<|>)((:<|>))
  , serveWithContextT
  , Context (..)
  )
import Control.Monad.Reader (runReaderT)

import CheckCheck.Contracts.API (API)
import WebAPI.Receipts (receiptsServer)
import WebAPI.Users (usersServer)
import WebAPI.AppM (AppM(runAppM), Env)
import WebAPI.Auth (getJwtSettings)
import WebAPI.Groups (groupsServer)

server :: ServerT API AppM
server
  =    receiptsServer
  :<|> groupsServer
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

