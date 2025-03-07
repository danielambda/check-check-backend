{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module WebAPI.Users.OutgoingRequests (outgoingRequestsServer, Dependencies) where

import Servant (ServerT)

import CheckCheck.Contracts.Users (AuthenticatedUser)
import CheckCheck.Contracts.Users.OutgoingRequests (OutgoingRequestsAPI)
import qualified WebAPI.Users.OutgoingRequests.Send as Send (Dependencies)
import WebAPI.Users.OutgoingRequests.Send (sendRequest)

type Dependencies m = (Send.Dependencies m)
outgoingRequestsServer :: Dependencies m => AuthenticatedUser -> ServerT OutgoingRequestsAPI m
outgoingRequestsServer = sendRequest
