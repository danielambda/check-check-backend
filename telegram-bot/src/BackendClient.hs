{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BackendClient
  ( ApiClient(..)
  , GroupsClient(..)
  , UsersClient(..)
  , ContactsClient(..)
  , OutgointRequestsClient(..)
  , apiClient
  , BackendClientM
  ) where

import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.UUID (UUID)
import Servant.API ((:<|>)(..), NoContent)
import Servant.Auth.Client (Token)
import Servant.Client (ClientM, client, hoistClient)

import CheckCheck.Contracts.API (API)
import CheckCheck.Contracts.Groups (CreateGroupReqBody, GroupResp)
import CheckCheck.Contracts.Receipts (ReceiptResp)
import CheckCheck.Contracts.Users (UserResp)
import CheckCheck.Contracts.Users.OutgoingRequests (SendRequestReqBody, RequestResp)
import CheckCheck.Contracts.Users.Contacts (CreateContactReqBody, ContactResp)
import ClientMUtils (AsKeyedClientM (..))

newtype BackendClientM a = BackendClientM { unBackendClientM :: ClientM a }
  deriving (Functor, Applicative, Monad)

instance AsKeyedClientM BackendClientM "backend" where
  asClientM = unBackendClientM

data ApiClient = ApiClient
  { getReceipt :: Text -> BackendClientM ReceiptResp
  , mkGroupsClient :: Token -> GroupsClient
  , mkUsersClient :: Token -> UsersClient
  }

data GroupsClient = GroupsClient
  { createGroup :: CreateGroupReqBody -> BackendClientM GroupResp
  , getGroup :: UUID -> BackendClientM GroupResp
  , getAllGroups :: BackendClientM [GroupResp]
  }

data UsersClient = UsersClient
  { getMe :: BackendClientM UserResp
  , contactsClient :: ContactsClient
  , outgoingRequestsClient :: OutgointRequestsClient
  }

data ContactsClient = ContactsClient
  { createContact :: CreateContactReqBody -> BackendClientM NoContent
  , getContacts :: BackendClientM [ContactResp]
  , deletaContact :: UUID -> BackendClientM NoContent
  }

newtype OutgointRequestsClient = OutgointRequestsClient
  { sendRequest :: SendRequestReqBody -> BackendClientM [RequestResp] }

apiClient :: ApiClient
apiClient = ApiClient{..}
  where
    api = Proxy :: Proxy API
    nt :: ClientM a -> BackendClientM a
    nt = BackendClientM

    getReceipt :<|> groupsClient :<|> usersClient = hoistClient api nt $ client api

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

