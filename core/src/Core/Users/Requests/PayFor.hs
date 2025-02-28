{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Core.Users.Requests.PayFor
  ( Data(..)
  , Dependencies
  , payForRequest
  , Error(..)
  ) where

import Optics ((%), (^?))

import Data.Typeable (eqT, (:~:)(..))

import Core.Users.Domain.UserId (SomeUserId)
import Core.Users.MonadClasses.Repository (UsersRepository (getSomeUserFromRepo))
import Core.Users.Budget.Domain.Budget (RoundingData, Budget)
import Core.Users.Requests.MonadClasses.Repository
  (RequestsRepository (getRequestFromRepo, markRequestCompletedInRepo))
import Core.Users.Requests.Domain.RequestStatus (RequestStatus(..))
import Core.Users.Requests.Domain.RequestId (RequestId, SomeRequestId (SomeRequestId))
import Core.Users.Requests.Domain.Request (SomeRequest(SomeRequest), Request, payFor)

data Data = Data
  { recipientId :: SomeUserId
  , requestId :: RequestId 'Pending
  , mRoundingData :: Maybe RoundingData
  }

data Error
  = UserDoesNotExist SomeUserId
  | UserDoesNotHaveBudget SomeUserId
  | RequestDoesNotExist (RequestId 'Pending)
  | RequestIsNotPending (RequestId 'Pending)

type Dependencies m = (UsersRepository m, RequestsRepository m)
payForRequest :: Dependencies m => Data -> m (Either Error Budget)
payForRequest Data{ recipientId, requestId, mRoundingData } =
  getSomeUserFromRepo recipientId >>= \case
    Nothing -> return $ Left $ UserDoesNotExist recipientId
    Just recipient -> getRequestFromRepo (SomeRequestId requestId) >>= \case
      Nothing -> return $ Left $ RequestDoesNotExist requestId
      Just (SomeRequest (request :: Request status)) -> case eqT @status @'Pending of
        Nothing -> return $ Left $ RequestIsNotPending requestId
        Just Refl -> case recipient ^? #data % #mBudget of
          Nothing -> return $ Left $ UserDoesNotHaveBudget recipientId
          Just budget -> do
            let (paidRequest, budget') = payFor mRoundingData request budget
            markRequestCompletedInRepo paidRequest
            return $ Right budget'

