{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module WebAPI.Groups.OutgoingRequests (outgoingRequestsServer, Dependencies) where

import Servant (ServerT)

import CheckCheck.Contracts.Users (AuthenticatedUser)
import CheckCheck.Contracts.Users.OutgoingRequests (OutgoingRequestsAPI)
import qualified WebAPI.Groups.OutgoingRequests.Send as Send (Dependencies)
import WebAPI.Groups.OutgoingRequests.Send (sendRequest)
import Data.UUID (UUID)

type Dependencies m = (Send.Dependencies m)
outgoingRequestsServer :: Dependencies m => AuthenticatedUser -> UUID -> ServerT OutgoingRequestsAPI m
outgoingRequestsServer = sendRequest
