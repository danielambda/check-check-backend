{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Clients
  ( ApiClient(..)
  , GroupsClient(..)
  , UsersClient(..)
  , ContactsClient(..)
  , OutgointRequestsClient(..)
  , apiClient
  ) where

import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.UUID (UUID)
import Servant.API ((:<|>)(..), NoContent)
import Servant.Auth.Client (Token)
import Servant.Client (ClientM, client)

import CheckCheck.Contracts.API (API)
import CheckCheck.Contracts.Groups (CreateGroupReqBody, GroupResp)
import CheckCheck.Contracts.Receipts (ReceiptResp)
import CheckCheck.Contracts.Users (UserResp)
import CheckCheck.Contracts.Users.OutgoingRequests (SendRequestReqBody, RequestResp)
import CheckCheck.Contracts.Users.Contacts (CreateContactReqBody, ContactResp)

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
  , contactsClient :: ContactsClient
  , outgoingRequestsClient :: OutgointRequestsClient
  }

data ContactsClient = ContactsClient
  { createContact :: CreateContactReqBody -> ClientM ContactResp
  , getContacts :: ClientM [ContactResp]
  , deletaContact :: UUID -> ClientM NoContent
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
        getMe :<|> contactsClient' :<|> outgoingRequestsClient' :<|> _ = usersClient token

        contactsClient = ContactsClient{..}
          where
            getContacts :<|> createContact :<|> deletaContact = contactsClient'

        outgoingRequestsClient = OutgointRequestsClient{..}
          where
            sendRequest = outgoingRequestsClient'

