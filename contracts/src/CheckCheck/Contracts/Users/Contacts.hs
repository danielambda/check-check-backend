{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module CheckCheck.Contracts.Users.Contacts
  ( ContactsAPI
  , GetContacts
  , CreateContact
  , CreateContactReqBody(..)
  , ContactResp(..)
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.UUID (UUID)
import Servant.API ((:<|>), Get, JSON, ReqBody, (:>), Post)

import GHC.Generics (Generic)

import SmartPrimitives.TextLenRange (TextLenRange)
import SmartPrimitives.TextMaxLen (TextMaxLen)

type ContactsAPI
  =    GetContacts
  :<|> CreateContact

type GetContacts =
  Get '[JSON] [ContactResp]

type CreateContact =
  ReqBody '[JSON] CreateContactReqBody :> Post '[JSON] ContactResp

data CreateContactReqBody = CreateContactReqBody
  { contactUserId :: UUID
  , contactName :: Maybe (TextMaxLen 50)
  } deriving (Generic, ToJSON, FromJSON)

data ContactResp = ContactResp
  { contactUserId :: UUID
  , contactName :: Maybe (TextMaxLen 50)
  } deriving (Generic, ToJSON, FromJSON)

