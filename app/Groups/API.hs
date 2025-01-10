{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NamedFieldPuns #-}

module Groups.API (GroupsAPI, groupsServer) where

import Servant ((:>), (:<|>)((:<|>)), Capture, Get, JSON, ServerT, ReqBody, PostCreated)
import Data.UUID (UUID)
import Data.Text (Text)

import Control.Monad.IO.Class (MonadIO)

import Common.Persistence (MonadConnPoolReader)
import Groups.Types (Group (Group), GroupId (GroupId))
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Groups.Persistence (addGroupToDb, getGroupFromDb)
import Data.Maybe (fromMaybe)

type GroupsAPI = "groups"
  :> ( ReqBody '[JSON] CreateGroupReqBody :> PostCreated '[JSON] GroupId -- TODO use Post with no body with location header
  :<|> Capture "groupId" UUID :> Get '[JSON] (Maybe Group)
  )

data CreateGroupReqBody = CreateGroupReqBody
  { name :: Text
  , initialBudget :: Maybe Integer
  , initialMembers :: Maybe [UUID]
  } deriving (Generic, FromJSON)

groupsServer :: (MonadIO m, MonadConnPoolReader m) =>
                ServerT GroupsAPI m
groupsServer
  =    createGroup
  :<|> getGroup
  where
    createGroup :: (MonadIO m, MonadConnPoolReader m) =>
                   CreateGroupReqBody -> m GroupId
    createGroup CreateGroupReqBody{ name, initialBudget } = do
      -- TODO handle initialMembers
      let budget = fromMaybe 0 initialBudget
      let group = Group name budget
      addGroupToDb group

    getGroup :: (MonadIO m, MonadConnPoolReader m) =>
                UUID -> m (Maybe Group)
    getGroup = getGroupFromDb . GroupId

instance ToJSON GroupId
instance ToJSON Group
