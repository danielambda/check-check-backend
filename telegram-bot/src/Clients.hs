{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Clients
  ( ApiClient(..)
  , GroupsClient(..)
  , UsersClient(..)
  , OutgointRequestsClient(..)
  , apiClient
  ) where

import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.UUID (UUID)
import Servant.API ((:<|>)(..))
import Servant.Auth.Client (Token)
import Servant.Client (ClientM, client)

import CheckCheck.Contracts.API (API)
import CheckCheck.Contracts.Groups (CreateGroupReqBody, GroupResp)
import CheckCheck.Contracts.Receipts (ReceiptResp)
import CheckCheck.Contracts.Users (UserResp)
import CheckCheck.Contracts.Users.OutgoingRequests (SendRequestReqBody, RequestResp)

data ApiClient = ApiClient
  { getReceipt :: Text -> ClientM ReceiptResp
  , mkGroupsClient :: Token -> GroupsClient
  , mkUsersClient :: Token -> UsersClient
  }

data GroupsClient = GroupsClient
  { createGroup :: CreateGroupReqBody -> ClientM GroupResp
  , getGroup :: UUID -> ClientM GroupResp
  , getAllGroups :: ClientM [GroupResp]
  }

data UsersClient = UsersClient
  { getMe :: ClientM UserResp
  , outgoingRequestsClient :: OutgointRequestsClient
  }

newtype OutgointRequestsClient = OutgointRequestsClient
  { sendRequest :: SendRequestReqBody -> ClientM [RequestResp] }

apiClient :: ApiClient
apiClient = ApiClient{..}
  where
    getReceipt :<|> groupsClient :<|> usersClient = client $ Proxy @API

    mkGroupsClient token = GroupsClient{..}
      where
        createGroup :<|> getGroup :<|> getAllGroups :<|> _ = groupsClient token

    mkUsersClient token = UsersClient{..}
      where
        getMe :<|> _ :<|> outgoingRequestsClient' :<|> _ = usersClient token

        outgoingRequestsClient = OutgointRequestsClient{..}
          where
            sendRequest = outgoingRequestsClient'

