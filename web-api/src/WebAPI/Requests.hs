{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module WebAPI.Requests (requestsServer) where

import Control.Monad.Reader (asks, MonadIO (liftIO))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Servant ((:<|>)((:<|>)), ServerT)

import Control.Concurrent (readMVar, modifyMVar_)
import Control.Monad (void, forM)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (fromMaybe)
import qualified Data.List.NonEmpty as NonEmpty (toList)
import qualified Data.Map as Map

import CheckCheck.Contracts.Requests (RequestResp(..), RequestItemResp(..), RequestItemReqBody(..), RequestsAPI, PostRequestReqBody (..))
import CheckCheck.Contracts.Budget (BudgetResp)
import Core.Common.MonadClasses.MonadUTCTime (MonadUTCTime(currentTime))
import Core.Common.MonadClasses.MonadUUID (MonadUUID(newUUID))
import Infrastructure.Common.Persistence (MonadPG, executeMany, execute, withTransaction, query_, queryMaybe, query)
import WebAPI.AppM (AppM, Env (budgetAmountMVar))
import WebAPI.Budget (Budget (Budget), budgetToResp)

requestToResp :: Request -> RequestResp
requestToResp Request{items=items',..} = RequestResp{..}
  where
    isPending = status == Pending
    items = fmap itemToResp items'
    itemToResp RequestItem{..} = RequestItemResp{..}

requestItemFromReqBody :: RequestItemReqBody -> RequestItem
requestItemFromReqBody RequestItemReqBody{..} = RequestItem{..}

data RequestStatus = Pending | Completed
  deriving (Eq)

data RequestItem = RequestItem
  { name :: Text
  , price :: Integer
  }

data Request = Request
  { requestId :: UUID
  , items :: NonEmpty RequestItem
  , createdAt :: UTCTime
  , status :: RequestStatus
  }

requestsServer :: ServerT RequestsAPI AppM
requestsServer
  =    postRequest
  :<|> getRequests
  :<|> completeRequest

postRequest :: PostRequestReqBody -> AppM RequestResp
postRequest (PostRequestReqBody items') = do
  requestId <- newUUID
  createdAt <- currentTime
  let req = Request
          { requestId
          , items = fmap requestItemFromReqBody  items'
          , createdAt
          , status = Pending
          }
  postReqeustToDb req
  return $ requestToResp req

getRequests :: AppM [RequestResp]
getRequests = do
  requests <- query_ [sql|
    SELECT request_id, created_at, is_pending
    FROM requests
  |]
  items <- query_ [sql|
    SELECT request_id, name, price
    FROM request_items
  |]

  let itemsMap = Map.fromListWith (++)
        [(reqId, [RequestItem name price]) | (reqId, name, price) <- items]

  forM requests $ \(reqId, createdAt, isPending) -> do
    let itemsList = fromMaybe (error $ "Request without items: " ++ show reqId)
                   (Map.lookup reqId itemsMap)
    let neItems = fromMaybe (error $ "Request without items: " ++ show reqId)
                 (nonEmpty itemsList)
    let status = if isPending then Pending else Completed
    let request = Request
          { requestId = reqId
          , items = neItems
          , createdAt = createdAt
          , status = status
          }
    return $ requestToResp request

getRequestFromDb :: MonadPG m => UUID -> m Request
getRequestFromDb reqId = do
  mReqMeta <- queryMaybe [sql|
    SELECT created_at, is_pending
    FROM requests
    WHERE request_id = ?
  |] (Only reqId)

  case mReqMeta of
    Nothing -> error $ "Request not found: " ++ show reqId
    Just (createdAt, isPending) -> do
      items <- query [sql|
        SELECT name, price
        FROM request_items
        WHERE request_id = ?
      |] (Only reqId)

      let domainItems = map (uncurry RequestItem) items

      case nonEmpty domainItems of
        Nothing -> error $ "Request has no items: " ++ show reqId
        Just nonEmptyItems -> do
          let status = if isPending then Pending else Completed
          return Request
            { requestId = reqId
            , items = nonEmptyItems
            , createdAt = createdAt
            , status = status
            }

completeRequestInDb :: MonadPG m => UUID -> m ()
completeRequestInDb reqId = void $ execute [sql|
  update requests set is_pending = false where request_id = ?
|] (Only reqId)

completeRequest :: UUID -> AppM BudgetResp
completeRequest reqId = do
  Request{items} <- getRequestFromDb reqId
  let reqMoneyTotal = sum $ fmap (\RequestItem{price} -> price) items
  budgetAmountMVar <- asks budgetAmountMVar
  budget <- liftIO $ do
    modifyMVar_ budgetAmountMVar (pure . subtract reqMoneyTotal)
    Budget <$> readMVar budgetAmountMVar
  completeRequestInDb reqId
  return $ budgetToResp budget

postReqeustToDb :: MonadPG m => Request -> m ()
postReqeustToDb Request {requestId, items, createdAt, status} = withTransaction $ do
  let f RequestItem{name, price} = (requestId, name, price)
  void $ execute [sql|
    insert into requests (request_id, created_at, is_pending) values (?, ?, ?)
  |] (requestId, createdAt, status == Pending)
  void $ executeMany [sql|
    insert into request_items (request_id, name, price) values (?, ?, ?)
  |] (f <$> NonEmpty.toList items)
