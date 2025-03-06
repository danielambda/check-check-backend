{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module WebAPI.Users.Get
  ( Dependencies
  , GetMe, getMe
  , UserResp(..), toResp
  ) where

import Servant (ServerT, JSON, Get, throwError, ServerError, err400)
import Data.Aeson (ToJSON)
import Data.UUID (UUID)
import Optics ((^?), (%), to, (<&>), (&))
import Control.Monad.Error.Class (MonadError)

import GHC.Generics (Generic)

import SmartPrimitives.TextLenRange (TextLenRange, mkTextLenRange)
import Core.Common.Operators ((^^.))
import Core.Users.Domain.UserType (UserType(Single))
import Core.Users.Domain.User (User)
import Core.Users.Domain.UserId (UserId(UserId))
import qualified Core.Users.GetSingle as Impl (getSingle, Dependencies)
import WebAPI.Users.Budget.Get (BudgetResp)
import qualified WebAPI.Users.Budget.Get as Budget (toResp)
import WebAPI.Auth (AuthenticatedUser (..))
import Core.Users.CreateExistingSingle (createExistingSingle)
import Core.Users.Domain.Primitives (Username(Username))

type GetMe =
  Get '[JSON] UserResp

data UserResp = UserResp
  { userId :: UUID
  , username :: TextLenRange 2 50
  , budget :: Maybe BudgetResp
  } deriving (Generic, ToJSON)

type Dependencies m = (Impl.Dependencies m, MonadError ServerError m)
getMe :: Dependencies m => AuthenticatedUser -> ServerT GetMe m
getMe AUser{ userId, username } = userId
   &  UserId
   &  Impl.getSingle
  <&> fmap toResp
  >>= maybe createMe return
  where
    createMe = case Username <$> mkTextLenRange username of
      Left _ -> throwError err400
      Right username' -> toResp <$> createExistingSingle (UserId userId) username'

toResp :: User 'Single -> UserResp
toResp user = UserResp
  { userId = user ^^. #userId
  , username = user ^^. #data % #username
  , budget = user ^? #data % #mBudget % to Budget.toResp
  }
