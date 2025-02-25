{-# LANGUAGE DataKinds #-}

module Core.Users.Requests.MonadClasses.Repository
  ( RequestsRepository(..)
  ) where

import Core.Users.Requests.Domain.RequestStatus (RequestStatus(..))
import Core.Users.Requests.Domain.Request (Request, SomeRequest)
import Core.Users.Domain.UserId (SomeUserId)

class Monad m => RequestsRepository m where
  addRequestToRepo :: Request 'Pending -> m ()
  getIncomingRequestsFromRepo :: SomeUserId -> m [SomeRequest]

