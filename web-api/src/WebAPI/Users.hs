{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module WebAPI.Users (UsersAPI, usersServer) where

import Servant (ServerT, (:<|>)((:<|>)), (:>))
import Servant.Auth.Server (AuthResult(Authenticated))

import WebAPI.Auth (Authenticated)
import WebAPI.Users.OutgoingRequests (OutgoingRequestsAPI, requestsServer)
import qualified WebAPI.Users.OutgoingRequests as OutgoingRequests (Dependencies)
import WebAPI.Users.CreateSingle (CreateUserSingle, createUserSingle)
import qualified WebAPI.Users.CreateSingle as CreateSingle (Dependencies)
import WebAPI.Users.Get (GetUser, getUser)
import qualified WebAPI.Users.Get as Get (Dependencies)
import WebAPI.Users.IncomingRequests (IncomingRequestsAPI, incomingRequestsServer)
import qualified WebAPI.Users.IncomingRequests as IncomingRequests (Dependencies)
import WebAPI.Users.Budget (BudgetAPI, budgetServer)

type UsersAPI
  =    Authenticated
       :> ( "outgoing-requests" :> OutgoingRequestsAPI
       :<|> "incoming-requests" :> IncomingRequestsAPI
       :<|> "budget" :> BudgetAPI
       )
  :<|> CreateUserSingle
  :<|> GetUser

type Dependencies m =
  ( CreateSingle.Dependencies m
  , Get.Dependencies m
  , OutgoingRequests.Dependencies m
  , IncomingRequests.Dependencies m
  )
usersServer :: (Dependencies m) => ServerT UsersAPI m
usersServer
  =    (\case
          Authenticated user
             ->   requestsServer user
             :<|> incomingRequestsServer user
             :<|> budgetServer user
          _ -> error "TODO you are unauthenticated btw"
       )
  :<|> createUserSingle
  :<|> getUser
