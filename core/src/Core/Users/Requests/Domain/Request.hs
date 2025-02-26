{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Core.Users.Requests.Domain.Request
  ( Request(..), newRequest
  , RequestItem(..), RequestItemIdentity(..)
  , SomeRequest(..)
  ) where

import Optics (makeFieldLabelsNoPrefix, LabelOptic (labelOptic), A_Getter, to)
import Data.Text (Text)
import Data.Time (UTCTime)

import Data.List.NonEmpty (NonEmpty)
import Data.Typeable ((:~:)(Refl), eqT, Typeable)

import SmartPrimitives.Positive (Positive)
import Core.Common.MonadClasses.MonadUTCTime (MonadUTCTime(currentTime))
import Core.Common.MonadClasses.MonadUUID (MonadUUID)
import Core.Users.Domain.UserId (SomeUserId)
import Core.Users.Requests.Domain.RequestStatus (RequestStatus (..))
import Core.Users.Requests.Domain.RequestId (RequestId, newRequestId)
import Core.Common.Domain.RubKopecks (RubKopecks)

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
  , price :: Positive RubKopecks
  }

data RequestItemIdentity
  = TextIdentity Text
  | ReceiptItemNameIdentity Text

makeFieldLabelsNoPrefix ''Request
makeFieldLabelsNoPrefix ''RequestItem

instance (Typeable (s :: RequestStatus), a ~ RequestStatus, a ~ b)
      => LabelOptic "status" A_Getter (Request s) (Request s) a b where
  labelOptic = to $ \_ -> case eqT @s @'Pending of
    Just Refl -> Pending
    Nothing -> case eqT @s @'Done of
      Just Refl -> Done
      Nothing -> error "unreachable"

newRequest :: (MonadUUID m, MonadUTCTime m)
           => SomeUserId -> SomeUserId -> NonEmpty RequestItem -> m (Request 'Pending)
newRequest senderId recipientId items = do
  requestId <- newRequestId
  createdAt <- currentTime
  return Request{..}

data SomeRequest where
  SomeRequest :: Typeable status => Request status -> SomeRequest

