{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module WebAPI.Users.Get
  ( Dependencies
  , GetUser, getUser
  , UserSingleResp(..), toResp
  ) where

import Servant (ServerT, (:>), JSON, Get, Capture, throwError, err404, ServerError)
import Data.Aeson (ToJSON)
import Data.UUID (UUID)
import Optics ((^?), (%), to, (<&>), (&))
import Control.Monad.Error.Class (MonadError)

import GHC.Generics (Generic)

import SmartPrimitives.TextLenRange (TextLenRange)
import Core.Common.Operators ((^^.))
import Core.Users.Domain.UserType (UserType(Single))
import Core.Users.Domain.User (User)
import Core.Users.Domain.UserId (UserId(UserId))
import qualified Core.Users.Get as Impl (get, Dependencies)
import WebAPI.Users.Budget.Get (BudgetResp)
import qualified WebAPI.Users.Budget.Get as Budget (toResp)

type GetUser =
  Capture "userId" UUID :> Get '[JSON] UserSingleResp

data UserSingleResp = UserSingleResp
  { userId :: UUID
  , username :: TextLenRange 2 50
  , budget :: Maybe BudgetResp
  } deriving (Generic, ToJSON)

type Dependencies m = (Impl.Dependencies m, MonadError ServerError m)
getUser ::  Dependencies m => ServerT GetUser m
getUser userId = userId
   &  UserId
   &  Impl.get
  <&> fmap toResp
  >>= maybe (throwError err404) return

toResp :: User 'Single -> UserSingleResp
toResp user = UserSingleResp
  { userId = user ^^. #userId
  , username = user ^^. #data % #username
  , budget = user ^? #data % #mBudget % to Budget.toResp
  }
