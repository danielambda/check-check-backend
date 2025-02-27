{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}

module Core.Users.Requests.MarkCompleted
  ( Dependencies
  , Data(..)
  , markRequestCompleted
  , Error(..)
  ) where

import Data.Typeable (eqT, (:~:)(..))

import Core.Users.Domain.UserId (SomeUserId)
import Core.Users.MonadClasses.Repository (UsersRepository (userExistsInRepo))
import Core.Users.Requests.MonadClasses.Repository
  (RequestsRepository (getRequestFromRepo, markRequestCompletedInRepo))
import Core.Users.Requests.Domain.RequestStatus (RequestStatus(..))
import Core.Users.Requests.Domain.RequestId (RequestId, SomeRequestId (SomeRequestId))
import Core.Users.Requests.Domain.Request (SomeRequest(SomeRequest), markCompleted, Request)

data Data = Data
  { recipientId :: SomeUserId
  , requestId :: RequestId 'Pending
  }

data Error
  = UserDoesNotExist SomeUserId
  | RequestDoesNotExist (RequestId 'Pending)
  | RequestIsNotPending (RequestId 'Pending)

type Dependencies m = (UsersRepository m, RequestsRepository m)
markRequestCompleted :: Dependencies m => Data -> m (Either Error ())
markRequestCompleted Data{ recipientId, requestId } = do
  recipientExists <- userExistsInRepo recipientId
  if not recipientExists then
    return $ Left $ UserDoesNotExist recipientId
  else
    getRequestFromRepo (SomeRequestId requestId) >>= \case
      Nothing -> return $ Left $ RequestDoesNotExist requestId
      Just (SomeRequest (request :: Request status)) -> case eqT @status @'Pending of
        Nothing -> return $ Left $ RequestIsNotPending requestId
        Just Refl -> do
          let completedRequest = markCompleted request
          markRequestCompletedInRepo completedRequest
          return $ Right ()
