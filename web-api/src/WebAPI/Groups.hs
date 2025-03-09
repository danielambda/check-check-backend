{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module WebAPI.Groups (Dependencies, groupsServer) where

import Servant (HasServer(ServerT), (:<|>) ((:<|>)))

import WebAPI.Groups.Create ( createGroup)
import qualified WebAPI.Groups.Create as Create (Dependencies)
import WebAPI.Groups.Get (getGroup)
import qualified WebAPI.Groups.Get as Get (Dependencies)
import WebAPI.Groups.GetAll (getAllGroups)
import qualified WebAPI.Groups.GetAll as GetAll (Dependencies)
import CheckCheck.Contracts.Groups (GroupsAPI)
import Servant.Auth.Server (AuthResult(Authenticated))
import WebAPI.Groups.OutgoingRequests (outgoingRequestsServer)
import qualified WebAPI.Groups.OutgoingRequests as OutgoingRequest (Dependencies)
import WebAPI.Groups.IncomingRequests (incomingRequestsServer)
import qualified WebAPI.Groups.IncomingRequests as IncomingRequest (Dependencies)
import WebAPI.Groups.Budget (budgetServer)
import qualified WebAPI.Groups.Budget as Budget (Dependencies)

type Dependencies m =
  ( Get.Dependencies m
  , Create.Dependencies m
  , GetAll.Dependencies m
  , OutgoingRequest.Dependencies m
  , IncomingRequest.Dependencies m
  , Budget.Dependencies m
  )
groupsServer :: Dependencies m => ServerT GroupsAPI m
groupsServer (Authenticated user)
  =    createGroup user
  :<|> getGroup user
  :<|> getAllGroups user
  :<|> (\groupId
       ->   outgoingRequestsServer user groupId
       :<|> incomingRequestsServer user groupId
       :<|> budgetServer user groupId
       )

groupsServer _ = error "unauthorized 401"

