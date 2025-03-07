{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module WebAPI.Groups.IncomingRequests (Dependencies, incomingRequestsServer) where

import Servant (ServerT, (:<|>) ((:<|>)))

import CheckCheck.Contracts.Users.IncomingRequests (IncomingRequestsAPI)
import CheckCheck.Contracts.Users (AuthenticatedUser)
import qualified WebAPI.Groups.IncomingRequests.GetMany as GetMany (Dependencies)
import WebAPI.Groups.IncomingRequests.GetMany (getIncomingRequests)
import qualified WebAPI.Groups.IncomingRequests.Complete as Complete (Dependencies)
import WebAPI.Groups.IncomingRequests.Complete (completeIncomingRequest)
import Data.UUID (UUID)

type Dependencies m = (GetMany.Dependencies m, Complete.Dependencies m)
incomingRequestsServer :: Dependencies m => AuthenticatedUser -> UUID -> ServerT IncomingRequestsAPI m
incomingRequestsServer user groupId
  =    getIncomingRequests user groupId
  :<|> completeIncomingRequest user groupId
