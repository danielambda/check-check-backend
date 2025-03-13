{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module WebAPI.Users.Contacts (Dependencies, contactsServer) where

import Servant (ServerT, (:<|>) ((:<|>)))

import CheckCheck.Contracts.Users.Contacts (ContactsAPI)
import CheckCheck.Contracts.Users (AuthenticatedUser)
import WebAPI.Users.Contacts.GetAll (getContacts)
import qualified WebAPI.Users.Contacts.GetAll as GetAll (Dependencies)
import WebAPI.Users.Contacts.Create (createContact)
import qualified WebAPI.Users.Contacts.Create as Create (Dependencies)
import WebAPI.Users.Contacts.Delete (deleteContact)
import qualified WebAPI.Users.Contacts.Delete as Delete (Dependencies)

type Dependencies m =
  ( GetAll.Dependencies m
  , Create.Dependencies m
  , Delete.Dependencies m
  )
contactsServer :: Dependencies m => AuthenticatedUser -> ServerT ContactsAPI m
contactsServer auser
  =    getContacts auser
  :<|> createContact auser
  :<|> deleteContact auser
