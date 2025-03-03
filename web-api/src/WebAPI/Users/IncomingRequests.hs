{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module WebAPI.Users.IncomingRequests (Dependencies, IncomingRequestsAPI, incomingRequestsServer) where

import Servant (ServerT, (:<|>) ((:<|>)))

import WebAPI.Auth (AuthenticatedUser)
import WebAPI.Users.IncomingRequests.GetMany (getIncomingRequests, GetIncomingRequests)
import qualified WebAPI.Users.IncomingRequests.GetMany as GetMany (Dependencies)
import WebAPI.Users.IncomingRequests.Complete (completeIncomingRequest, CompleteIncomingRequest)
import qualified WebAPI.Users.IncomingRequests.Complete as Complete (Dependencies)

type IncomingRequestsAPI
  =    GetIncomingRequests
  :<|> CompleteIncomingRequest

type Dependencies m = (GetMany.Dependencies m, Complete.Dependencies m)
incomingRequestsServer :: Dependencies m => AuthenticatedUser -> ServerT IncomingRequestsAPI m
incomingRequestsServer user
  =    getIncomingRequests user
  :<|> completeIncomingRequest user
