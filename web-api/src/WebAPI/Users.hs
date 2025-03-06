{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module WebAPI.Users (UsersAPI, usersServer) where

import Servant (ServerT, (:<|>)((:<|>)), (:>))
import Servant.Auth.Server (AuthResult(Authenticated))

import WebAPI.Auth (Authenticated)
import WebAPI.Users.OutgoingRequests (OutgoingRequestsAPI, requestsServer)
import qualified WebAPI.Users.OutgoingRequests as OutgoingRequests (Dependencies)
import WebAPI.Users.Get (GetMe, getMe)
import qualified WebAPI.Users.Get as Get (Dependencies)
import WebAPI.Users.IncomingRequests (IncomingRequestsAPI, incomingRequestsServer)
import qualified WebAPI.Users.IncomingRequests as IncomingRequests (Dependencies)
import WebAPI.Users.Budget (BudgetAPI, budgetServer)

type UsersAPI = Authenticated
  :> ( "outgoing-requests" :> OutgoingRequestsAPI
  :<|> "incoming-requests" :> IncomingRequestsAPI
  :<|> "budget" :> BudgetAPI
  :<|> "me" :> GetMe
  )

type Dependencies m =
  ( Get.Dependencies m
  , OutgoingRequests.Dependencies m
  , IncomingRequests.Dependencies m
  )
usersServer :: (Dependencies m) => ServerT UsersAPI m
usersServer (Authenticated user)
  =    requestsServer user
  :<|> incomingRequestsServer user
  :<|> budgetServer user
  :<|> getMe user
usersServer _ = error "Unauthorized but I do now know how to make this 401"
