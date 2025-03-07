{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module WebAPI.Auth
  ( getJwtSettings
  , Authenticated
  , ensureUserExistsInRepo
  ) where

import Crypto.JWT (Alg(HS256))
import Servant.Auth (Auth, JWT)
import Servant.Auth.Server
  ( defaultJWTSettings
  , JWTSettings (jwtAlg)
  , fromSecret
  )
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Base64 (decode)

import System.Environment (getEnv)
import Control.Monad (unless, void)
import Core.Users.Domain.Primitives (Username(Username))
import Core.Users.Domain.UserId (UserId(UserId), SomeUserId (SomeUserId))
import CheckCheck.Contracts.Users (AuthenticatedUser (..))
import Core.Users.MonadClasses.Repository (UsersRepository(userExistsInRepo))
import Core.Users.CreateExistingSingle (createExistingSingle)

type Authenticated = Auth '[JWT] AuthenticatedUser

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

ensureUserExistsInRepo :: UsersRepository m => AuthenticatedUser -> m ()
ensureUserExistsInRepo AUser{ userId, username } = do
  userExists <- userExistsInRepo $ SomeUserId userId
  unless userExists $
    void $ createExistingSingle (UserId userId) (Username username)
