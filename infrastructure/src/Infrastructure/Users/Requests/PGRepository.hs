{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Infrastructure.Users.Requests.PGRepository
  ( RequestsRepositoryT(..)
  , createRequestsTable
  , createRequestItemsTable
  ) where

import Database.PostgreSQL.Simple (ToRow, Only (..), FromRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Database.PostgreSQL.Simple.ToField (ToField (toField), Action (Escape))
import Optics ((%), (^..), folded, to)
import Data.UUID (UUID)
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.HashMap.Strict as HashMap (elems, fromListWith)

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import GHC.Generics (Generic)
import Data.String (IsString(fromString))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (append, singleton)

import SmartPrimitives.Positive (Positive)
import Core.Common.Operators ((^^.))
import Core.Common.Domain.Currency (Currency(Kopecks), SomeCurrency(SomeCurrency))
import Core.Users.Domain.UserId (SomeUserId (SomeUserId), UserId (UserId))
import Core.Users.Requests.Domain.RequestId (RequestId(RequestId))
import Core.Users.Requests.MonadClasses.Repository (RequestsRepository(..))
import Core.Users.Requests.Domain.Request
  (Request(..), RequestItem(..), RequestItemIdentity(..), SomeRequest (SomeRequest))
import Core.Users.Requests.Domain.RequestStatus (RequestStatus(..))
import Infrastructure.Common.Persistence.Internal.ByteStringParsableEnum
  (ByteStringParsableEnum, mkEnumFieldParser)
import Infrastructure.Common.Persistence
  (MonadConnReader, execute, executeMany, withTransaction, query, execute_)

newtype RequestsRepositoryT m a = RequestsRepositoryT
  { runRequestsRepositoryT :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadConnReader)

instance (MonadIO m, MonadConnReader m) => RequestsRepository (RequestsRepositoryT m) where
  addRequestToRepo = addRequestToDb
  getIncomingRequestsFromRepo = getIncomingRequestsFromDb

addRequestToDb :: (MonadIO m, MonadConnReader m) => Request 'Pending -> m ()
addRequestToDb request =
  let (dbRequest, dbRequestItems) = toDb request
  in withTransaction $ do
    void $ execute [sql|
      INSERT INTO requests (requestId, senderId, recipientId, createdAt, isPending)
      VALUES (?, ?, ?, ?, ?)
    |] dbRequest
    void $ executeMany [sql|
      INSERT INTO requestItems (requestId, identityTag, identityContents, quantity, price)
      VALUES (?, ?, ?, ?, ?)
    |] dbRequestItems

getIncomingRequestsFromDb :: (MonadIO m, MonadConnReader m) => SomeUserId -> m [SomeRequest]
getIncomingRequestsFromDb (SomeUserId(UserId recipientId)) =
  map (uncurry toDomain) . normalize <$> query [sql|
    SELECT requestId, senderId, createdAt, isPending
                    , identityTag, identityContents
                    , quantity, price
    FROM requests
    NATURAL JOIN requestItems
    WHERE recipientId = ?
  |] (Only recipientId)
  where
    normalize :: [( UUID, UUID, UTCTime, Bool
                        , DbRequestItemIdentityTag, Text
                        , Positive Double, Positive Integer
                  )]
              -> [(DbRequest, NonEmpty DbRequestItem)]
    normalize
      = HashMap.elems
      . HashMap.fromListWith merge
      . map processTuple
      where
        merge (request, items1) (_, items2) =
          (request, NonEmpty.append items1 items2)
        processTuple ( requestId, senderId, createdAt, isPending
                     , identityTag, identityContents
                     , quantity, price
                     )
          = (requestId, (DbRequest{..}, NonEmpty.singleton DbRequestItem{..}))

toDb :: Request 'Pending -> (DbRequest, [DbRequestItem])
toDb request@Request{ createdAt } =
  let
    requestId = request ^^. #requestId
    senderId = request ^^. #senderId
    recipientId = request ^^. #recipientId
    isPending = True
    requestItems = request ^.. #items % folded % to (dbRequestItem requestId)
  in (DbRequest{..}, requestItems)
  where
    dbRequestItem requestId item@RequestItem{..} =
      let (identityTag, identityContents) = case identity of
            TextIdentity t -> (TextRequestItemIdentity, t)
            ReceiptItemNameIdentity t -> (ReceiptItemNameTextRequestItemIdentity, t)
      in DbRequestItem {price = item ^^. #price , ..}

toDomain :: DbRequest -> NonEmpty DbRequestItem -> SomeRequest
toDomain DbRequest{..} dbRequestItems = SomeRequest $ Request
  { requestId = RequestId requestId
  , senderId    = SomeUserId $ UserId senderId
  , recipientId = SomeUserId $ UserId recipientId
  , items = toDomain' <$> dbRequestItems
  , ..
  }
  where
    toDomain' :: DbRequestItem -> RequestItem
    toDomain' DbRequestItem { identityTag, identityContents, quantity, price } = RequestItem
      { identity = case identityTag of
        TextRequestItemIdentity -> TextIdentity identityContents
        ReceiptItemNameTextRequestItemIdentity -> ReceiptItemNameIdentity identityContents
      , price = SomeCurrency $ Kopecks price
      , ..
      }

data DbRequest = DbRequest
  { requestId :: UUID
  , senderId :: UUID
  , recipientId :: UUID
  , createdAt :: UTCTime
  , isPending :: Bool
  } deriving (Generic, FromRow, ToRow)

createRequestsTable :: (MonadIO m, MonadConnReader m) => m ()
createRequestsTable = void $ execute_ [sql|
  CREATE TABLE IF NOT EXISTS requests
  ( requestId UUID PRIMARY KEY NOT NULL
  , senderId UUID NOT NULL REFERENCES users(userId)
  , recipientId UUID NOT NULL REFERENCES users(userId)
  , createdAt TIMESTAMPTZ NOT NULL
  , isPending BOOL NOT NULL
  )
|]

data DbRequestItem = DbRequestItem
  { requestId :: UUID
  , identityTag :: DbRequestItemIdentityTag
  , identityContents :: Text
  , quantity :: Positive Double
  , price :: Positive Integer
  } deriving (Generic, FromRow, ToRow)

data DbRequestItemIdentityTag
  = TextRequestItemIdentity
  | ReceiptItemNameTextRequestItemIdentity
  deriving (Show, Generic, ByteStringParsableEnum)
instance FromField DbRequestItemIdentityTag where
  fromField = mkEnumFieldParser "requestitemidentitytag"
instance ToField DbRequestItemIdentityTag where
  toField = Escape . fromString . show

createRequestItemsTable :: (MonadIO m, MonadConnReader m) => m ()
createRequestItemsTable = void $ execute_ [sql|
  CREATE TYPE requestItemIdentityTag AS ENUM
  ( 'TextRequestItemIdentity'
  , 'ReceiptItemNameTextRequestItemIdentity'
  );

  CREATE TABLE IF NOT EXISTS requestItems
  ( requestId UUID NOT NULL REFERENCES requests(requestId)
  , identityTag requestItemIdentityTag NOT NULL
  , identityContents TEXT NOT NULL
  , quantity REAL NOT NULL
  , price INTEGER NOT NULL
  )
|]
