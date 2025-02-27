{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module WebAPI.Users.IncomingRequests (Dependencies, IncomingRequestsAPI, incomingRequestsServer) where

import Servant (ServerT, (:<|>) ((:<|>)))
import Data.UUID (UUID)

import WebAPI.Users.IncomingRequests.GetMany (getIncomingRequests, GetIncomingRequests)
import qualified WebAPI.Users.IncomingRequests.GetMany as GetMany (Dependencies)
import WebAPI.Users.IncomingRequests.Complete (completeIncomingRequest, CompleteIncomingRequest)
import qualified WebAPI.Users.IncomingRequests.Complete as Complete (Dependencies)

type IncomingRequestsAPI
  =    GetIncomingRequests
  :<|> CompleteIncomingRequest

type Dependencies m = (GetMany.Dependencies m, Complete.Dependencies m)
incomingRequestsServer :: Dependencies m => UUID -> ServerT IncomingRequestsAPI m
incomingRequestsServer recipientId
  =    getIncomingRequests recipientId
  :<|> completeIncomingRequest recipientId
