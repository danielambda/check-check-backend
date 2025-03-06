{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module WebAPI.Groups.Get
  ( Dependencies
  , GetGroup, getGroup
  , GroupResp, toResp
  ) where

import Servant (ServerT, Capture, (:>), JSON, Get, err404, ServerError, err403)
import Data.Aeson (ToJSON)
import Data.UUID (UUID)
import Optics ((%), to, (^?), summing)

import GHC.Generics (Generic)
import Control.Monad.Error.Class (throwError, MonadError)

import SmartPrimitives.TextLenRange (TextLenRange)
import Core.Common.Operators ((^^.), (^^..))
import Core.Users.Domain.UserId (UserId(UserId))
import Core.Users.Domain.User (User)
import Core.Users.Domain.UserType (UserType(Group))
import qualified Core.Users.GetGroup as Impl (getGroup, Dependencies)
import WebAPI.Users.Budget.Get (BudgetResp)
import qualified WebAPI.Users.Budget.Get as Budget (toResp)
import WebAPI.Auth (AuthenticatedUser (..))

type GetGroup =
  Capture "groupId" UUID :> Get '[JSON] GroupResp

data GroupResp = GroupResp
  { groupId :: UUID
  , name :: TextLenRange 2 50
  , ownerId :: UUID
  , otherUserIds :: [UUID]
  , budget :: Maybe BudgetResp
  } deriving (Generic, ToJSON)

type Dependencies m = (Impl.Dependencies m, MonadError ServerError m)
getGroup :: Dependencies m => AuthenticatedUser -> ServerT GetGroup m
getGroup AUser{ userId } groupId = Impl.getGroup (UserId groupId) >>= \case
  Nothing -> throwError err404
  Just group -> if userId `elem` group ^^.. (#ownerId `summing` #otherUserIds)
    then return $ toResp group
    else throwError err403

toResp :: User 'Group -> GroupResp
toResp group = GroupResp
  { groupId = group ^^. #userId
  , name = group ^^. #data % #username
  , ownerId = group ^^. #ownerId
  , otherUserIds = group ^^.. #otherUserIds
  , budget = group ^? #data % #mBudget % to Budget.toResp
  }

