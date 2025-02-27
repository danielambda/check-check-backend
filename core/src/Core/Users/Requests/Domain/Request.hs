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
{-# LANGUAGE OverloadedLabels #-}

module Core.Users.Requests.Domain.Request
  ( Request(..)
  , RequestItem(..), RequestItemIdentity(..)
  , SomeRequest(..)
  , newRequest, markCompleted, payFor
  ) where

import Optics
  ( makeFieldLabelsNoPrefix
  , LabelOptic (labelOptic), A_Getter, to
  , (^.)
  )
import Data.Text (Text)
import Data.Time (UTCTime)

import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty)
import Data.Typeable ((:~:)(Refl), eqT, Typeable)

import SmartPrimitives.Positive (Positive, sumPositive)
import Core.Common.MonadClasses.MonadUTCTime (MonadUTCTime(currentTime))
import Core.Common.MonadClasses.MonadUUID (MonadUUID)
import Core.Users.Domain.UserId (SomeUserId)
import Core.Users.Requests.Domain.RequestStatus (RequestStatus (..))
import Core.Users.Requests.Domain.RequestId (RequestId, newRequestId)
import Core.Common.Domain.RubKopecks (RubKopecks)
import Core.Users.Budget.Domain.Budget (Budget, BudgetLowerBoundStatus, applyDelta)
import Core.Common.Operators ((^^.))
import Optics.Getter (view)

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
    Nothing -> case eqT @s @'Completed of
      Just Refl -> Completed
      Nothing -> error "unreachable"

instance (a ~ Positive RubKopecks, a ~ b)
      => LabelOptic "priceSum" A_Getter (Request s) (Request s) a b where
  labelOptic = to $ \request ->
    sumPositive $ view #price <$> request ^. #items

newRequest :: (MonadUUID m, MonadUTCTime m)
           => SomeUserId -> SomeUserId -> NonEmpty RequestItem -> m (Request 'Pending)
newRequest senderId recipientId items = do
  requestId <- newRequestId
  createdAt <- currentTime
  return Request{..}

-- TODO introduce gadt with completion data like UTCTime and completion action
markCompleted :: Request 'Pending -> Request 'Completed
markCompleted = coerce

payFor :: Request 'Pending -> Budget -> (Request 'Completed, Budget, BudgetLowerBoundStatus)
payFor request budget = let
  (budget', budgetLBStatus) = applyDelta (request ^^. #priceSum) budget
  completedRequest = markCompleted request
  in (completedRequest, budget', budgetLBStatus)

data SomeRequest where
  SomeRequest :: Typeable status => Request status -> SomeRequest

