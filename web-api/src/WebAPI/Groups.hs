{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module WebAPI.Groups (GroupsAPI, Dependencies, groupsServer) where

import Servant (HasServer(ServerT), (:<|>) ((:<|>)))

import WebAPI.Auth (AuthenticatedUser)
import WebAPI.Groups.Get (GetGroup, getGroup)
import qualified WebAPI.Groups.Get as Get (Dependencies)
import WebAPI.Groups.Create (CreateGroup, createGroup)
import qualified WebAPI.Groups.Create as Create (Dependencies)

type GroupsAPI
  =    GetGroup
  :<|> CreateGroup

type Dependencies m = (Get.Dependencies m, Create.Dependencies m)
groupsServer :: Dependencies m => AuthenticatedUser -> ServerT GroupsAPI m
groupsServer user
  =    getGroup user
  :<|> createGroup user

