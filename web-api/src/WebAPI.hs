{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module WebAPI (mkApp, initDb) where

import Control.Monad.Reader (runReaderT, asks, MonadIO (liftIO))

import WebAPI.AppM (AppM(runAppM), Env (budgetAmountMVar))
import Servant
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Infrastructure.Common.Persistence (MonadPG, executeMany, execute_, execute, withTransaction, query_, queryMaybe, query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Control.Monad (void, forM)
import Data.UUID (UUID)
import Data.Time (UTCTime)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Core.Common.MonadClasses.MonadUUID (MonadUUID(newUUID))
import Core.Common.MonadClasses.MonadUTCTime (MonadUTCTime(currentTime))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Concurrent (readMVar, modifyMVar_)
import Database.PostgreSQL.Simple (Only(..))

type API
  =    "requests" :> RequestsAPI
  :<|> "budget" :> BudgetAPI

type RequestsAPI
  =    PostRequest
  :<|> GetRequests
  :<|> Capture "requestId" UUID :> "complete" :> Post '[JSON] BudgetResp

type PostRequest = ReqBody '[JSON] PostRequestReqBody :> Post '[JSON] RequestResp
type GetRequests = Get '[JSON] [RequestResp]

type BudgetAPI = GetBudget :<|> ApplyBudgetDelta

type GetBudget = Get '[JSON] BudgetResp
type ApplyBudgetDelta = ReqBody '[JSON] Integer :> Patch '[JSON] BudgetResp

data RequestItemReqBody = RequestItemReqBody
  { name :: Text
  , price :: Integer
  } deriving (Generic, FromJSON)

newtype PostRequestReqBody = PostRequestReqBody (NonEmpty RequestItemReqBody)
  deriving stock Generic
  deriving anyclass FromJSON

data RequestResp = RequestResp
  { requestId :: UUID
  , items :: NonEmpty RequestItemResp
  , createdAt :: UTCTime
  , isPending :: Bool
  } deriving (Generic, ToJSON)

data RequestItemResp = RequestItemResp
  { name :: Text
  , price :: Integer
  } deriving (Generic, ToJSON)

requestToResp :: Request -> RequestResp
requestToResp Request{items=items',..} = RequestResp{..}
  where
    isPending = status == Pending
    items = fmap itemToResp items'
    itemToResp RequestItem{..} = RequestItemResp{..}

budgetToResp :: Budget -> BudgetResp
budgetToResp Budget{..} = BudgetResp{..}

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

newtype BudgetResp = BudgetResp
  { amount :: Integer }
  deriving stock Generic
  deriving anyclass ToJSON

newtype Budget = Budget
  { amount :: Integer }

server :: ServerT API AppM
server
  =    requestsServer
  :<|> budgetServer

requestsServer :: ServerT RequestsAPI AppM
requestsServer
  =    postRequest
  :<|> getRequests
  :<|> completeRequest

budgetServer :: ServerT BudgetAPI AppM
budgetServer = getBudget :<|> applyBudgetDelta

getBudget :: AppM BudgetResp
getBudget = do
  budgetAmountMVar <- asks budgetAmountMVar
  budgetAmount <- liftIO $ readMVar budgetAmountMVar
  let budget = Budget budgetAmount
  return $ budgetToResp budget

applyBudgetDelta :: Integer -> AppM BudgetResp
applyBudgetDelta delta = do
  budgetAmountMVar <- asks budgetAmountMVar
  budgetAmount <- liftIO $ do
    modifyMVar_ budgetAmountMVar (pure . (+delta))
    readMVar budgetAmountMVar
  let budget = Budget budgetAmount
  return $ budgetToResp budget

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

initDb :: MonadPG m => m ()
initDb = void $ execute_ [sql|
  create table if not exists requests
  ( request_id UUID NOT NULL PRIMARY KEY
  , created_at TIMESTAMPTZ NOT NULL
  , is_pending BOOL NOT NULL
  );

  create table if not exists request_items
  ( request_id UUID NOT NULL REFERENCES requests(request_id)
  , name TEXT NOT NULL
  , price INTEGER NOT NULL
  )
|]

mkApp :: Env -> IO Application
mkApp env = do
  return $ serve api $ hoistServer api nt server
  where
    api = Proxy :: Proxy API

    nt :: AppM a -> Handler a
    nt = flip runReaderT env . runAppM

