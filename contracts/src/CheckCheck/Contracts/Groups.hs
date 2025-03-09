{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}

module CheckCheck.Contracts.Groups
  ( GroupsAPI
  , CreateGroup
  , GetGroup
  , GetAllGroups
  , CreateGroupReqBody(..)
  , GroupResp(..)
  ) where

import Servant.API ((:>), (:<|>), Capture, Get, Post, JSON, ReqBody)
import CheckCheck.Contracts.Users.Budget (BudgetAPI, BudgetResp)
import CheckCheck.Contracts.Users.OutgoingRequests (OutgoingRequestsAPI)
import CheckCheck.Contracts.Users.IncomingRequests (IncomingRequestsAPI)
import CheckCheck.Contracts.Users (Authenticated)
import Data.UUID (UUID)
import SmartPrimitives.TextLenRange (TextLenRange)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

type GroupsAPI = Authenticated
  :> ( CreateGroup
  :<|> GetGroup
  :<|> GetAllGroups
  :<|> Capture "groupId" UUID
       :> ( "outgoing-requests" :> OutgoingRequestsAPI
       :<|> "incoming-requests" :> IncomingRequestsAPI
       :<|> "budget" :> BudgetAPI
       )
  )

type CreateGroup =
  ReqBody '[JSON] CreateGroupReqBody :> Post '[JSON] GroupResp

type GetGroup =
  Capture "groupId" UUID :> Get '[JSON] GroupResp

type GetAllGroups =
  Get '[JSON] [GroupResp]

data CreateGroupReqBody = CreateGroupReqBody
  { name :: TextLenRange 2 50
  , otherUserIds :: Maybe [UUID]
  } deriving (Generic, ToJSON, FromJSON)

data GroupResp = GroupResp
  { groupId :: UUID
  , name :: TextLenRange 2 50
  , ownerId :: UUID
  , otherUserIds :: [UUID]
  , budget :: Maybe BudgetResp
  } deriving (Generic, ToJSON, FromJSON)


