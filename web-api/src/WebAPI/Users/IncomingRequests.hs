{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module WebAPI.Users.IncomingRequests (Dependencies, IncomingRequestsAPI, incomingRequestsServer) where

import Servant (ServerT)
import Data.UUID (UUID)

import WebAPI.Users.IncomingRequests.GetMany (getIncomingRequests, GetIncomingRequests, Dependencies)

type IncomingRequestsAPI = GetIncomingRequests

incomingRequestsServer :: Dependencies m => UUID -> ServerT GetIncomingRequests m
incomingRequestsServer = getIncomingRequests
