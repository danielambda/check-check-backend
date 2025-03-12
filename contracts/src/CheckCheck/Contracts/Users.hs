{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}

module CheckCheck.Contracts.Users
  ( UsersAPI
  , GetMe
  , UserResp(..)
  , AuthenticatedUser(..)
  , Authenticated
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import Servant.API ((:>), (:<|>), Get, JSON)
import Servant.Auth (Auth, JWT)
import Servant.Auth.JWT (FromJWT, ToJWT)

import GHC.Generics (Generic)

import SmartPrimitives.TextLenRange (TextLenRange)
import CheckCheck.Contracts.Users.Budget (BudgetAPI, BudgetResp)
import CheckCheck.Contracts.Users.OutgoingRequests (OutgoingRequestsAPI)
import CheckCheck.Contracts.Users.IncomingRequests (IncomingRequestsAPI)
import CheckCheck.Contracts.Users.Contacts (ContactsAPI)

type Authenticated = Auth '[JWT] AuthenticatedUser

data AuthenticatedUser = AUser
  { userId :: UUID
  , username :: TextLenRange 2 50
  } deriving (Generic, FromJSON, ToJSON, FromJWT, ToJWT)

type UsersAPI = Authenticated
  :> ( "me" :> GetMe
  :<|> "contacts" :> ContactsAPI
  :<|> "outgoing-requests" :> OutgoingRequestsAPI
  :<|> "incoming-requests" :> IncomingRequestsAPI
  :<|> "budget" :> BudgetAPI
  )

type GetMe =
  Get '[JSON] UserResp

data UserResp = UserResp
  { userId :: UUID
  , username :: TextLenRange 2 50
  , budget :: Maybe BudgetResp
  } deriving (Generic, ToJSON, FromJSON)

