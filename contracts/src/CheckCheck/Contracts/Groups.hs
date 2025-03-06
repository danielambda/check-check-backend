{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}

module CheckCheck.Contracts.Groups
  ( GroupsAPI
  , GetGroup
  , CreateGroup
  , CreateGroupReqBody(..)
  , GroupResp(..)
  ) where

import Servant.API ((:>), (:<|>), Capture, Get, JSON, ReqBody)
import CheckCheck.Contracts.Users.Budget (BudgetAPI, BudgetResp)
import CheckCheck.Contracts.Users.OutgoingRequests (OutgoingRequestsAPI)
import CheckCheck.Contracts.Users.IncomingRequests (IncomingRequestsAPI)
import CheckCheck.Contracts.Users (Authenticated)
import Data.UUID (UUID)
import SmartPrimitives.TextLenRange (TextLenRange)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

type GroupsAPI
  =    Authenticated :> Capture "groupId" UUID
    :> ( "outgoing-requests" :> OutgoingRequestsAPI
    :<|> "incoming-requests" :> IncomingRequestsAPI
    :<|> "budget" :> BudgetAPI
    )
  :<|> GetGroup
  :<|> CreateGroup

type GetGroup =
  Capture "groupId" UUID :> Get '[JSON] GroupResp

type CreateGroup =
  ReqBody '[JSON] CreateGroupReqBody :> Get '[JSON] GroupResp

data CreateGroupReqBody = CreateGroupReqBody
  { name :: TextLenRange 2 50
  , otherUserIds :: [UUID]
  } deriving (Generic, ToJSON, FromJSON)

data GroupResp = GroupResp
  { groupId :: UUID
  , name :: TextLenRange 2 50
  , ownerId :: UUID
  , otherUserIds :: [UUID]
  , budget :: Maybe BudgetResp
  } deriving (Generic, ToJSON, FromJSON)


