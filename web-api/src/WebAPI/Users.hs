{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module WebAPI.Users (usersServer) where

import Servant (ServerT, (:<|>)((:<|>)))
import Servant.Auth.Server (AuthResult(..))

import CheckCheck.Contracts.Users (UsersAPI)
import WebAPI.Users.Get (getMe)
import qualified WebAPI.Users.Get as Get (Dependencies)
import WebAPI.Users.Contacts (contactsServer)
import qualified WebAPI.Users.Contacts as Contacts (Dependencies)
import WebAPI.Users.IncomingRequests (incomingRequestsServer)
import qualified WebAPI.Users.IncomingRequests as IncomingRequests (Dependencies)
import WebAPI.Users.OutgoingRequests (outgoingRequestsServer)
import qualified WebAPI.Users.OutgoingRequests as OutgoingRequests (Dependencies)
import WebAPI.Users.Budget (budgetServer)
import qualified WebAPI.Users.Budget as Budget (Dependencies)

type Dependencies m =
  ( Get.Dependencies m
  , Contacts.Dependencies m
  , OutgoingRequests.Dependencies m
  , IncomingRequests.Dependencies m
  , Budget.Dependencies m
  )
usersServer :: (Dependencies m) => ServerT UsersAPI m
usersServer (Authenticated user)
  =    getMe user
  :<|> contactsServer user
  :<|> outgoingRequestsServer user
  :<|> incomingRequestsServer user
  :<|> budgetServer user
usersServer _ = error "Unauthorized but I do now know how to make this 401"
