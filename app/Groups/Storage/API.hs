{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Groups.Storage.API (GroupsStorageAPI, groupsStorageServer) where

import Servant (JSON, (:>), Get, HasServer (ServerT))

import Common.Persistence (MonadConnPoolReader)
import Goods.Types (Goods)
import Groups.Types (GroupId)
import Groups.Persistence (getStorageEntriesFromDb)

type GroupsStorageAPI = "storage" :> Get '[JSON] [(Goods, Double)]

groupsStorageServer :: MonadConnPoolReader m => GroupId -> ServerT GroupsStorageAPI m
groupsStorageServer groupId = getAll
  where
    getAll :: MonadConnPoolReader m => m [(Goods, Double)]
    getAll = getStorageEntriesFromDb groupId
