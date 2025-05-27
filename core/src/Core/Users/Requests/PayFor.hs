{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Core.Users.Requests.PayFor
  ( Data(..)
  , Dependencies
  , payForRequest
  , Error(..)
  ) where

import Optics ((%), (^?), (.~), (&), (%~))

import Control.Monad (forM_)
import Data.Typeable (eqT, (:~:)(..))

import Core.Users.Domain.UserId (SomeUserId)
import Core.Users.MonadClasses.Repository
  (UsersRepository (getSomeUserFromRepo, updateSomeUserInRepo))
import Core.Users.Requests.MonadClasses.Repository
  (RequestsRepository (getRequestFromRepo, markRequestCompletedInRepo))
import Core.Users.Budget.Domain.Budget (RoundingData, Budget)
import Core.Users.Requests.Domain.RequestStatus (RequestStatus(..))
import Core.Users.Requests.Domain.RequestId (RequestId, SomeRequestId (SomeRequestId))
import Core.Users.Requests.Domain.Request (SomeRequest(SomeRequest), Request (..), payFor, receiveMoneyFor)

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
            let (paidRequest@Request{senderId}, budget') = payFor mRoundingData request budget
            markRequestCompletedInRepo paidRequest
            mSender <- getSomeUserFromRepo senderId
            forM_ mSender $ \sender ->
              updateSomeUserInRepo $
                sender & #data % #mBudget %~ receiveMoneyFor mRoundingData request
            updateSomeUserInRepo $ recipient & #data % #mBudget .~ budget
            return $ Right budget'

