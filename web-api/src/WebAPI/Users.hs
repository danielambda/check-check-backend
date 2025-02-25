{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module WebAPI.Users (UsersAPI, usersServer) where

import Servant (ServerT, (:<|>)((:<|>)), (:>), Capture)
import Data.UUID (UUID)

import WebAPI.Users.OutgoingRequests (OutgoingRequestsAPI, requestsServer)
import qualified WebAPI.Users.OutgoingRequests as OutgoingRequests (Dependencies)
import WebAPI.Users.CreateSingle (CreateUserSingle, createUserSingle)
import qualified WebAPI.Users.CreateSingle as CreateSingle (Dependencies)
import WebAPI.Users.Get (GetUser, getUser)
import qualified WebAPI.Users.Get as Get (Dependencies)
import WebAPI.Users.IncomingRequests (IncomingRequestsAPI, incomingRequestsServer)

type UsersAPI
  = Capture "userId" UUID
    :> ( "outgoing-requests" :> OutgoingRequestsAPI
    :<|> "incoming-requests" :> IncomingRequestsAPI
    )
  :<|> CreateUserSingle
  :<|> GetUser

type Dependencies m =
  ( CreateSingle.Dependencies m
  , Get.Dependencies m
  , OutgoingRequests.Dependencies m
  )
usersServer :: (Dependencies m) => ServerT UsersAPI m
usersServer
  = (\userId
    ->   requestsServer userId
    :<|> incomingRequestsServer userId
    )
  :<|> createUserSingle
  :<|> getUser
