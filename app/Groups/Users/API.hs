{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Groups.Users.API (InGroupUsersAPI, inGroupUsersServer) where

import Servant ((:>), Capture, JSON, ServerT, ReqBody, NoContent (NoContent), PostNoContent, DeleteNoContent, (:<|>) ((:<|>)))

import Shared.Persistence (MonadConnPoolReader)
import Shared.Types.Positive (Positive)
import Goods.Types (GoodsId)
import Users.Types (UserId)
import Groups.Types (GroupId)
import Groups.Persistence (increaseGoodsQuantityDb, decreaseGoodsQuantityDb)

type InGroupUsersAPI = "users" :> Capture "userId" UserId
  :> "goods"
    :> ( ReqBody '[JSON] [(GoodsId, Positive Double)] :> PostNoContent
    :<|> ReqBody '[JSON] [(GoodsId, Positive Double)] :> DeleteNoContent
    )

inGroupUsersServer :: MonadConnPoolReader m
                   => GroupId -> ServerT InGroupUsersAPI m
inGroupUsersServer groupId userId
  =    increaseGoodsQuantity userId
  :<|> decreaseGoodsQuantity userId
  where
    increaseGoodsQuantity :: MonadConnPoolReader m
                          => UserId -> [(GoodsId, Positive Double)] -> m NoContent
    increaseGoodsQuantity _ deltasList =
      NoContent <$ increaseGoodsQuantityDb deltasList groupId

    decreaseGoodsQuantity :: MonadConnPoolReader m
                          => UserId -> [(GoodsId, Positive Double)] -> m NoContent
    decreaseGoodsQuantity _ deltasList =
      NoContent <$ decreaseGoodsQuantityDb deltasList groupId
