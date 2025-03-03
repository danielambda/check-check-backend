{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}

module WebAPI.Users.IncomingRequests.GetMany
  ( Impl.Dependencies
  , GetIncomingRequests, getIncomingRequests
  ) where

import Servant (ServerT, JSON, Get)
import Data.Aeson (ToJSON)
import Optics ((^.), (&))
import Data.UUID (UUID)
import Data.Time (UTCTime)
import Data.Text (Text)

import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)

import SmartPrimitives.Positive (Positive)
import Core.Common.Operators ((^^.))
import Core.Users.Domain.UserId (SomeUserId(SomeUserId), UserId (UserId))
import Core.Users.Requests.Domain.RequestStatus (RequestStatus(Pending))
import Core.Users.Requests.Domain.Request (SomeRequest (..), Request (..), RequestItem (..), RequestItemIdentity (..))
import qualified Core.Users.Requests.GetIncoming as Impl (getIncoming, Dependencies)
import WebAPI.Auth (AuthenticatedUser (AUser, userId))

type GetIncomingRequests =
  Get '[JSON] [RequestResp]

data RequestResp = RequestResp
  { requestId :: UUID
  , senderId :: UUID
  , recipientId :: UUID
  , items :: NonEmpty RequestItemResp
  , createdAt :: UTCTime
  , isPending :: Bool
  } deriving (Generic, ToJSON)

data RequestItemResp = RequestItemResp
  { identity :: Text
  , quantity :: Positive Double
  , price :: Positive Integer
  } deriving (Generic, ToJSON)

getIncomingRequests :: Impl.Dependencies m => AuthenticatedUser -> ServerT GetIncomingRequests m
getIncomingRequests AUser{ userId } =
  fmap toResp <$> Impl.getIncoming (SomeUserId $ UserId userId)

toResp :: SomeRequest -> RequestResp
toResp (SomeRequest request@Request{..}) = RequestResp
  { requestId = request ^^. #requestId
  , senderId = request ^^. #senderId
  , recipientId = request ^^. #recipientId
  , items = toResp' <$> items
  , isPending = request ^. #status == Pending
  , ..
  }
  where
    toResp' :: RequestItem -> RequestItemResp
    toResp' RequestItem{..} = RequestItemResp
      { identity = identity & \case
        TextIdentity t -> t
        ReceiptItemNameIdentity t -> t
      , price = price ^. #posValue
      , ..
      }
