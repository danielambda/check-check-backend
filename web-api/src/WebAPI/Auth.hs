{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module WebAPI.Auth (getJwtSettings, Authenticated, AuthenticatedUser(..)) where

import Crypto.JWT (Alg(HS256))
import Servant.Auth (Auth, JWT)
import Servant.Auth.Server
  ( defaultJWTSettings
  , JWTSettings (jwtAlg)
  , FromJWT, ToJWT
  , fromSecret
  )
import Data.UUID (UUID)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Base64 (decode)

import GHC.Generics (Generic)
import System.Environment (getEnv)

type Authenticated = Auth '[JWT] AuthenticatedUser

newtype AuthenticatedUser = AUser
  { userId :: UUID
  } deriving (Generic, FromJSON, ToJSON, FromJWT, ToJWT)

getJwtSettings :: IO JWTSettings
getJwtSettings = do
  key <- fromSecret <$> jwtSecret
  return $ (defaultJWTSettings key){ jwtAlg = Just HS256 }

jwtSecret :: IO ByteString
jwtSecret = do
  base64Secret <- getEnv "JWT_SECRET"
  case decode (pack base64Secret) of
    Right secret -> return secret
    Left _ -> error "Invalid JWT_SECRET format"
