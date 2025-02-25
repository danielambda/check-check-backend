{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Core.Users.Requests.Domain.Request
  ( Request(..), newRequest
  , RequestItem(..), RequestItemIdentity(..)
  , SomeRequest(..)
  ) where

import Optics (makeFieldLabelsNoPrefix)
import Data.Text (Text)
import Data.Time (UTCTime)

import Data.List.NonEmpty (NonEmpty)

import SmartPrimitives.Positive (Positive)
import Core.Common.MonadClasses.MonadUTCTime (MonadUTCTime(currentTime))
import Core.Common.MonadClasses.MonadUUID (MonadUUID)
import Core.Users.Domain.UserId (SomeUserId)
import Core.Users.Requests.Domain.RequestStatus (RequestStatus (Pending))
import Core.Users.Requests.Domain.RequestId (RequestId, newRequestId)
import Core.Common.Domain.Currency (SomeCurrency)

data Request (status :: RequestStatus) = Request
  { requestId :: RequestId status
  , senderId :: SomeUserId
  , recipientId :: SomeUserId
  , items :: NonEmpty RequestItem
  , createdAt :: UTCTime
  }

data RequestItem = RequestItem
  { identity :: RequestItemIdentity
  , quantity :: Positive Double
  , price :: SomeCurrency
  }

data RequestItemIdentity
  = TextIdentity Text
  | ReceiptItemNameIdentity Text

makeFieldLabelsNoPrefix ''Request
makeFieldLabelsNoPrefix ''RequestItem

newRequest :: (MonadUUID m, MonadUTCTime m)
           => SomeUserId -> SomeUserId -> NonEmpty RequestItem -> m (Request 'Pending)
newRequest senderId recipientId items = do
  requestId <- newRequestId
  createdAt <- currentTime
  return Request{..}

data SomeRequest where
  SomeRequest :: Request status -> SomeRequest

