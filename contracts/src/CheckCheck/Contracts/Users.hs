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

import Servant.Auth (Auth, JWT)
import Data.UUID (UUID)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Servant.Auth.JWT (FromJWT, ToJWT)
import Servant.API ((:>), (:<|>), Get, JSON)
import SmartPrimitives.TextLenRange (TextLenRange)
import CheckCheck.Contracts.Users.Budget (BudgetAPI, BudgetResp)
import CheckCheck.Contracts.Users.OutgoingRequests (OutgoingRequestsAPI)
import CheckCheck.Contracts.Users.IncomingRequests (IncomingRequestsAPI)

type Authenticated = Auth '[JWT] AuthenticatedUser

data AuthenticatedUser = AUser
  { userId :: UUID
  , username :: Text
  } deriving (Generic, FromJSON, ToJSON, FromJWT, ToJWT)

type UsersAPI = Authenticated
  :> ( "outgoing-requests" :> OutgoingRequestsAPI
  :<|> "incoming-requests" :> IncomingRequestsAPI
  :<|> "budget" :> BudgetAPI
  :<|> "me" :> GetMe
  )

type GetMe =
  Get '[JSON] UserResp

data UserResp = UserResp
  { userId :: UUID
  , username :: TextLenRange 2 50
  , budget :: Maybe BudgetResp
  } deriving (Generic, ToJSON, FromJSON)

