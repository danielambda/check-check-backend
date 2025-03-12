{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module WebAPI.Users.IncomingRequests (Dependencies, incomingRequestsServer) where

import Servant (ServerT, (:<|>) ((:<|>)))

import WebAPI.Users.IncomingRequests.GetAll (getIncomingRequests)
import qualified WebAPI.Users.IncomingRequests.GetAll as GetAll (Dependencies)
import WebAPI.Users.IncomingRequests.Complete (completeIncomingRequest)
import qualified WebAPI.Users.IncomingRequests.Complete as Complete (Dependencies)
import CheckCheck.Contracts.Users.IncomingRequests (IncomingRequestsAPI)
import CheckCheck.Contracts.Users (AuthenticatedUser)

type Dependencies m = (GetAll.Dependencies m, Complete.Dependencies m)
incomingRequestsServer :: Dependencies m => AuthenticatedUser -> ServerT IncomingRequestsAPI m
incomingRequestsServer user
  =    getIncomingRequests user
  :<|> completeIncomingRequest user
