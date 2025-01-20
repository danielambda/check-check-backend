{-# LANGUAGE FlexibleInstances #-}

module Shared.Persistence
  ( mkEnumFieldParser, sql, Only(Only)
  , MonadConnPoolReader , askConnPool
  , query, query_
  , execute, execute_
  , queryMaybe
  , querySingleField
  , executeMany
  , withTransaction
  ) where

import Database.PostgreSQL.Simple
  ( Query, Connection, ToRow, FromRow
  , fromOnly, Only(Only)
  , ResultError (Incompatible, UnexpectedNull, ConversionFailed)
  )
import Database.PostgreSQL.Simple.FromField
  ( FromField, FieldParser
  , typename, returnError
  )
import qualified Database.PostgreSQL.Simple as PG
  ( query, query_, execute_
  , execute, executeMany
  , withTransaction
  )
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask))

import Data.Maybe (listToMaybe)
import Data.Int (Int64)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Data.Data (Typeable)
import Shared.ByteStringParsableEnum (ByteStringParsableEnum, parseEnumBS)
import Data.ByteString (ByteString)

class MonadIO m => MonadConnPoolReader m where
  askConnPool :: m Connection

instance MonadIO m => MonadConnPoolReader (ReaderT Connection m) where
  askConnPool = ask

mkEnumFieldParser :: (Typeable a, ByteStringParsableEnum a) => ByteString -> FieldParser a
mkEnumFieldParser enumName field mdata = do
  n <- typename field
  if n /= enumName
  then returnError Incompatible field ""
  else case mdata of
    Nothing -> returnError UnexpectedNull field ""
    Just bs -> case parseEnumBS bs of
      Nothing -> returnError ConversionFailed field (show bs)
      Just x  -> return x

liftToMonadConstraints :: MonadConnPoolReader m
                       => (Connection -> Query -> q -> IO a) -> (Query -> q -> m a)
liftToMonadConstraints f queryText params = do
  conn <- askConnPool
  liftIO $ f conn queryText params

liftToMonadConstraints_ :: MonadConnPoolReader m
                        => (Connection -> Query -> IO a) -> (Query -> m a)
liftToMonadConstraints_ f queryText = do
  conn <- askConnPool
  liftIO $ f conn queryText

query :: (MonadConnPoolReader m, ToRow q, FromRow r)
      => Query -> q -> m [r]
query = liftToMonadConstraints PG.query

query_ :: (MonadConnPoolReader m, FromRow r)
       => Query -> m [r]
query_ = liftToMonadConstraints_ PG.query_

execute :: (MonadConnPoolReader m, ToRow q)
        => Query -> q -> m Int64
execute = liftToMonadConstraints PG.execute

execute_ :: MonadConnPoolReader m
         => Query -> m Int64
execute_ = liftToMonadConstraints_ PG.execute_

queryMaybe :: (MonadConnPoolReader m, ToRow q, FromRow r)
           => Query -> q -> m (Maybe r)
queryMaybe = (fmap listToMaybe .) . query

querySingleField :: (MonadConnPoolReader m, ToRow q, FromField f)
                 => Query -> q -> m f
querySingleField = (fmap (fromOnly . head) .) . query

executeMany :: (MonadConnPoolReader m, ToRow q) => Query -> [q] -> m Int64
executeMany = liftToMonadConstraints PG.executeMany

withTransaction :: MonadConnPoolReader m => ReaderT Connection IO a -> m a
withTransaction actions = do
  conn <- askConnPool
  let actionsIO = actions `runReaderT` conn
  liftIO $ PG.withTransaction conn actionsIO
