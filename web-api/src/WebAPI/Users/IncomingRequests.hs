{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module WebAPI.Users.IncomingRequests (IncomingRequestsAPI, incomingRequestsServer) where

import Servant (ServerT)
import Data.UUID (UUID)

import WebAPI.Users.IncomingRequests.Get (getIncomingRequests, GetIncomingRequests, Dependencies)

type IncomingRequestsAPI = GetIncomingRequests

incomingRequestsServer :: Dependencies m => UUID -> ServerT GetIncomingRequests m
incomingRequestsServer = getIncomingRequests
