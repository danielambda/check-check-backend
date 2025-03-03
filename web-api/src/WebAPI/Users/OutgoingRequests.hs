{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module WebAPI.Users.OutgoingRequests (OutgoingRequestsAPI, requestsServer, Dependencies) where

import Servant (ServerT)

import WebAPI.Auth (AuthenticatedUser)
import qualified WebAPI.Users.OutgoingRequests.Send as Send (Dependencies)
import WebAPI.Users.OutgoingRequests.Send (SendRequest, sendRequest)

type OutgoingRequestsAPI = SendRequest

type Dependencies m = (Send.Dependencies m)
requestsServer :: Dependencies m => AuthenticatedUser -> ServerT SendRequest m
requestsServer = sendRequest
