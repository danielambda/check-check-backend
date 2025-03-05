{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module WebAPI.Users.Register
  ( Impl.Dependencies
  , RegisterUserReqBody(..)
  , RegisterUser, registerUser
  , RegisterUserResp(..)
  ) where

import Optics ((%))
import Servant (ServerT, (:>), JSON, Post, ReqBody)
import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)

import GHC.Generics (Generic)

import SmartPrimitives.TextLenRange (TextLenRange)
import Core.Common.Operators ((^^.))
import Core.Users.Domain.UserType (UserType(Single))
import Core.Users.Domain.User (User)
import qualified Core.Users.CreateSingle as Impl (createSingle, Dependencies, Data(..))

type RegisterUser =
  ReqBody '[JSON] RegisterUserReqBody :> Post '[JSON] RegisterUserResp

newtype RegisterUserReqBody = RegisterUserReqBody
  { username :: TextLenRange 2 50 }
  deriving (Generic, FromJSON)

data RegisterUserResp = RegisterUserResp
  { userId :: UUID
  , username :: TextLenRange 2 50
  } deriving (Generic, ToJSON)

registerUser :: Impl.Dependencies m => ServerT RegisterUser m
registerUser RegisterUserReqBody{ username } =
  toResp <$> Impl.createSingle Impl.Data{ mBudgetData = Nothing, .. }

toResp :: User 'Single -> RegisterUserResp
toResp user = RegisterUserResp
  { userId = user ^^. #userId
  , username = user ^^. #data % #username
  }
