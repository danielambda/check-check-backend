{-# LANGUAGE ConstraintKinds #-}

module Core.Users.Requests.GetIncoming (Dependencies, getIncoming) where

import Core.Users.Requests.Domain.Request (SomeRequest)
import Core.Users.Domain.UserId (SomeUserId)
import Core.Users.Requests.MonadClasses.Repository (RequestsRepository(getIncomingRequestsFromRepo))

type Dependencies m = RequestsRepository m
getIncoming :: Dependencies m => SomeUserId -> m [SomeRequest]
getIncoming = getIncomingRequestsFromRepo
