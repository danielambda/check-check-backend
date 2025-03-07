{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module WebAPI.Users.IncomingRequests (Dependencies, incomingRequestsServer) where

import Servant (ServerT, (:<|>) ((:<|>)))

import WebAPI.Users.IncomingRequests.GetMany (getIncomingRequests)
import qualified WebAPI.Users.IncomingRequests.GetMany as GetMany (Dependencies)
import WebAPI.Users.IncomingRequests.Complete (completeIncomingRequest)
import qualified WebAPI.Users.IncomingRequests.Complete as Complete (Dependencies)
import CheckCheck.Contracts.Users.IncomingRequests (IncomingRequestsAPI)
import CheckCheck.Contracts.Users (AuthenticatedUser)

type Dependencies m = (GetMany.Dependencies m, Complete.Dependencies m)
incomingRequestsServer :: Dependencies m => AuthenticatedUser -> ServerT IncomingRequestsAPI m
incomingRequestsServer user
  =    getIncomingRequests user
  :<|> completeIncomingRequest user
