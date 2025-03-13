{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-x-partial #-}

module Infrastructure.Common.Persistence
  ( MonadPG, askConn
  , query, queryMaybe, query_, querySingleField
  , execute, execute_
  , executeMany
  , withTransaction
  ) where

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Database.PostgreSQL.Simple (Query, Connection, ToRow, FromRow, Only (fromOnly))
import qualified Database.PostgreSQL.Simple as PG
  ( query, query_, execute_
  , execute, executeMany
  , withTransaction
  )

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple.FromField (FromField)

class MonadIO m => MonadPG m where
  askConn :: m Connection

instance MonadIO m => MonadPG (ReaderT Connection m) where
  askConn = ask

query :: (MonadPG m, ToRow q, FromRow r) => Query -> q -> m [r]
query = liftToMonadConstraints PG.query

queryMaybe :: (MonadPG m, ToRow q, FromRow r) => Query -> q -> m (Maybe r)
queryMaybe queryText q = listToMaybe <$> query queryText q

querySingleField :: (MonadPG m, ToRow q, FromField f) => Query -> q -> m f
querySingleField = (fmap (fromOnly . head) .) . query


query_ :: (MonadPG m, FromRow r) => Query -> m [r]
query_ = liftToMonadConstraints_ PG.query_

execute :: (MonadPG m, ToRow q) => Query -> q -> m Int64
execute = liftToMonadConstraints PG.execute

execute_ :: MonadPG m => Query -> m Int64
execute_ = liftToMonadConstraints_ PG.execute_

executeMany :: (MonadPG m, ToRow q) => Query -> [q] -> m Int64
executeMany = liftToMonadConstraints PG.executeMany

withTransaction :: MonadPG m => ReaderT Connection IO a -> m a
withTransaction actions = do
  conn <- askConn
  let actionsIO = actions `runReaderT` conn
  liftIO $ PG.withTransaction conn actionsIO

liftToMonadConstraints :: MonadPG m => (Connection -> Query -> q -> IO a) -> (Query -> q -> m a)
liftToMonadConstraints f queryText params = do
  conn <- askConn
  liftIO $ f conn queryText params

liftToMonadConstraints_ :: MonadPG m => (Connection -> Query -> IO a) -> (Query -> m a)
liftToMonadConstraints_ f queryText = do
  conn <- askConn
  liftIO $ f conn queryText

