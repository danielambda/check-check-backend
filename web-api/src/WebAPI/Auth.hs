{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module WebAPI.Auth (Authenticated, AuthenticatedUser(..), config) where

import Servant.Auth.Server (generateKey, defaultJWTSettings, defaultCookieSettings, JWTSettings, CookieSettings, BasicAuthCfg, BasicAuthData (BasicAuthData), AuthResult (Indefinite, Authenticated), FromJWT, ToJWT, FromBasicAuthData (fromBasicAuthData))
import Servant.Auth (Auth, JWT, BasicAuth)
import Servant (Context(..))
import Data.UUID (UUID, fromString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.Function ((&))
import Data.Maybe (fromJust)

type Authenticated = Auth '[JWT, BasicAuth] AuthenticatedUser

data AuthenticatedUser = AUser
  { userId :: UUID
  , username :: Text
  , isAdmin :: Bool
  } deriving (Generic, FromJSON, ToJSON, FromJWT, ToJWT)

instance FromBasicAuthData AuthenticatedUser where
  fromBasicAuthData = (&)

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthenticatedUser)

authCheck :: BasicAuthData -> IO (AuthResult AuthenticatedUser)
authCheck (BasicAuthData "danielamba" "changeme") = return $ Authenticated $ AUser
  { userId = fromJust $ fromString "67aa7086-9c2c-422b-ab6d-a53f91c41b4a"
  , username = "danielamba"
  , isAdmin = True
  }
authCheck (BasicAuthData _ _) = return Indefinite

config :: IO (Context [JWTSettings, CookieSettings, BasicAuthCfg])
config = do
  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
  let authCfg = authCheck
  return $ jwtCfg :. defaultCookieSettings :. authCfg :. EmptyContext

