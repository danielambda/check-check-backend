{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module WebAPI.Users (usersServer) where

import Servant (ServerT, (:<|>)((:<|>)))

import WebAPI.Users.OutgoingRequests (outgoingRequestsServer)
import qualified WebAPI.Users.OutgoingRequests as OutgoingRequests (Dependencies)
import WebAPI.Users.Get (getMe)
import qualified WebAPI.Users.Get as Get (Dependencies)
import WebAPI.Users.IncomingRequests (incomingRequestsServer)
import qualified WebAPI.Users.IncomingRequests as IncomingRequests (Dependencies)
import WebAPI.Users.Budget (budgetServer)
import CheckCheck.Contracts.Users (UsersAPI)
import Servant.Auth.Server (AuthResult(..))

type Dependencies m =
  ( Get.Dependencies m
  , OutgoingRequests.Dependencies m
  , IncomingRequests.Dependencies m
  )
usersServer :: (Dependencies m) => ServerT UsersAPI m
usersServer (Authenticated user)
  =    outgoingRequestsServer user
  :<|> incomingRequestsServer user
  :<|> budgetServer user
  :<|> getMe user
usersServer _ = error "Unauthorized but I do now know how to make this 401"
