{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module WebAPI.Groups.Create
  ( Dependencies
  , CreateGroup, createGroup
  ) where

import Servant (ServerT, (:>), JSON, Get, ServerError, ReqBody, err400)
import Data.Aeson (ToJSON)
import Data.UUID (UUID)

import GHC.Generics (Generic)
import Control.Monad.Error.Class (throwError, MonadError)

import SmartPrimitives.TextLenRange (TextLenRange)
import Core.Users.Domain.UserId (UserId(UserId))
import qualified Core.Users.CreateGroup as Impl (Data(..), createGroup, Dependencies, Error(..))
import WebAPI.Auth (AuthenticatedUser (..))
import WebAPI.Groups.Get (GroupResp, toResp)

type CreateGroup =
  ReqBody '[JSON] CreateGroupReqBody :> Get '[JSON] GroupResp

data CreateGroupReqBody = CreateGroupReqBody
  { name :: TextLenRange 2 50
  , otherUserIds :: [UUID]
  } deriving (Generic, ToJSON)

type Dependencies m = (Impl.Dependencies m, MonadError ServerError m)
createGroup :: Dependencies m => AuthenticatedUser -> ServerT CreateGroup m
createGroup AUser{ userId } CreateGroupReqBody{..} = do
  let data' = Impl.Data
        { Impl.name = name
        , Impl.ownerId = UserId userId
        , Impl.otherUserIds = UserId <$> otherUserIds
        }
  Impl.createGroup data' >>= \case
    Right group -> return $ toResp group
    Left (Impl.NonExistingGroupMembers _) -> throwError err400

