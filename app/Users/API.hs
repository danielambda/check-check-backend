{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}

module Users.API (UsersAPI, usersServer) where

import Servant ((:>), (:<|>)((:<|>)), PostCreated, Get, JSON, Capture, ReqBody, HasServer (ServerT))
import Data.Text (Text)
import Data.Aeson (FromJSON)

import GHC.Generics (Generic)

import Shared.Persistence (MonadConnPoolReader)
import Groups.Types (GroupId)
import Users.Types (User (User), UserId)
import Users.Persistence (addUserToDb, getUserFromDb)
import Data.Either (fromRight)

type UsersAPI = "users"
  :> ( ReqBody '[JSON] CreateUserReqBody :> PostCreated '[JSON] UserId
  :<|> Capture "userId" UserId :> Get '[JSON] (Maybe User)
  )

usersServer :: MonadConnPoolReader m =>
               ServerT UsersAPI m
usersServer
  =    createUser
  :<|> getUser
  where
    createUser :: MonadConnPoolReader m =>
                  CreateUserReqBody -> m UserId
    createUser CreateUserReqBody{ username, groupId } = do
      let user = User username groupId
      fromRight (error "TODO") <$> addUserToDb user

    getUser :: MonadConnPoolReader m =>
               UserId -> m (Maybe User)
    getUser = getUserFromDb

data CreateUserReqBody = CreateUserReqBody
  { username :: Text
  , groupId :: Maybe GroupId
  } deriving (Generic, FromJSON)

