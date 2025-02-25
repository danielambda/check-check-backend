{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module WebAPI.Users.IncomingRequests.Get
  ( Impl.Dependencies
  , GetIncomingRequests, getIncomingRequests) where

import Servant (ServerT, JSON, Get)
import Data.Aeson (ToJSON)

import GHC.Generics (Generic)

import qualified Core.Users.Requests.GetIncoming as Impl (getIncoming, Dependencies)
import Data.UUID (UUID)
import Core.Users.Domain.UserId (SomeUserId(SomeUserId), UserId (UserId))
import Data.Time (UTCTime)
import Data.List.NonEmpty (NonEmpty)
import SmartPrimitives.Positive (Positive)
import Data.Text (Text)
import Core.Users.Requests.Domain.Request (SomeRequest (..), Request (..), RequestItem (..), RequestItemIdentity (..))
import Core.Common.Operators ((^^.))
import Optics ((^.), (&))
import Core.Common.Domain.Currency (SomeCurrency(SomeCurrency), Currency (Kopecks))

type GetIncomingRequests =
  Get '[JSON] [RequestResp]

data RequestResp = RequestResp
  { requestId :: UUID
  , senderId :: UUID
  , recipientId :: UUID
  , items :: NonEmpty RequestItemResp
  , createdAt :: UTCTime
  } deriving (Generic, ToJSON)

data RequestItemResp = RequestItemResp
  { identity :: Text
  , quantity :: Positive Double
  , price :: Positive Integer
  } deriving (Generic, ToJSON)

getIncomingRequests :: Impl.Dependencies m => UUID -> ServerT GetIncomingRequests m
getIncomingRequests userId =
  fmap toResp <$> Impl.getIncoming (SomeUserId $ UserId userId)

toResp :: SomeRequest -> RequestResp
toResp (SomeRequest request@Request{..}) = RequestResp
  { requestId = request ^^. #requestId
  , senderId = request ^^. #senderId
  , recipientId = request ^^. #recipientId
  , items = toResp' <$> items
  , ..
  }
  where
    toResp' :: RequestItem -> RequestItemResp
    toResp' RequestItem{ price = SomeCurrency (Kopecks price), ..} = RequestItemResp
      { identity = identity & \case
        TextIdentity t -> t
        ReceiptItemNameIdentity t -> t
      , ..
      }
