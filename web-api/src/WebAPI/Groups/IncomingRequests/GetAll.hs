{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}

module WebAPI.Groups.IncomingRequests.GetAll
  ( Impl.Dependencies
  , GetIncomingRequests, getIncomingRequests
  ) where

import Servant (ServerT)
import Optics ((^.))

import Core.Common.Operators ((^^.))
import Core.Users.Domain.UserId (SomeUserId(SomeUserId))
import Core.Users.Requests.Domain.RequestStatus (RequestStatus(Pending))
import Core.Users.Requests.Domain.Request (SomeRequest (..), Request (..), RequestItem (..), RequestItemIdentity (..))
import qualified Core.Users.Requests.GetIncoming as Impl (getIncoming, Dependencies)
import CheckCheck.Contracts.Users.IncomingRequests (GetIncomingRequests, RequestResp (..), RequestItemResp (..))
import CheckCheck.Contracts.Users (AuthenticatedUser(..))
import Data.UUID (UUID)

getIncomingRequests :: Impl.Dependencies m => AuthenticatedUser -> UUID -> ServerT GetIncomingRequests m
getIncomingRequests AUser{} groupId =
  fmap toResp <$> Impl.getIncoming (SomeUserId groupId)

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
      { identity = case identity of
        TextIdentity t -> t
        ReceiptItemNameIdentity t -> t
      , price = price ^. #posValue
      , ..
      }
