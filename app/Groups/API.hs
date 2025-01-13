{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}

module Groups.API (GroupsAPI, groupsServer) where

import Servant ((:>), (:<|>)((:<|>)), Capture, Get, JSON, ServerT, ReqBody, PostCreated)
import Data.Text (Text)
import Data.Aeson (FromJSON)

import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

import Common.Persistence (MonadConnPoolReader, withTransaction)
import Users.Persistence (setUserGroupId)
import Users.Types (UserId)
import Groups.Types (Group (Group), GroupId)
import Groups.Persistence (addGroupToDb, getGroupFromDb)
import Groups.Users.API (InGroupUsersAPI, inGroupUsersServer)
import Groups.Storage.API (GroupsStorageAPI, groupsStorageServer)

type GroupsAPI = "groups"
  :> ( ReqBody '[JSON] CreateGroupReqBody :> PostCreated '[JSON] GroupId -- TODO use Post with no body with location header
  :<|> Capture "groupId" GroupId
    :> ( InGroupUsersAPI
    :<|> GroupsStorageAPI
    :<|> Get '[JSON] (Maybe Group)
    )
  )

groupsServer :: MonadConnPoolReader m
             => ServerT GroupsAPI m
groupsServer
  =    createGroup
  :<|> \groupId
    ->   inGroupUsersServer groupId
    :<|> groupsStorageServer groupId
    :<|> getGroup groupId
  where
    createGroup :: MonadConnPoolReader m
                => CreateGroupReqBody -> m GroupId
    createGroup CreateGroupReqBody{ name, initialBudget, creatorId } = do
      let budget = fromMaybe 0 initialBudget
      let group = Group name budget
      withTransaction $ do
        groupId <- addGroupToDb group
        setUserGroupId creatorId groupId
        return groupId

    getGroup :: MonadConnPoolReader m =>
                GroupId -> m (Maybe Group)
    getGroup = getGroupFromDb

data CreateGroupReqBody = CreateGroupReqBody
  { name :: Text
  , initialBudget :: Maybe Integer
  , creatorId :: UserId
  } deriving (Generic, FromJSON)

