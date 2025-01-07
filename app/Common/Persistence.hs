module Common.Persistence
  ( MonadConnPoolReader , askConnPool
  , query, query_
  , execute, execute_
  , queryMaybe, queryMaybe_
  , executeMany
  ) where

import Database.PostgreSQL.Simple (Query, Connection, ToRow, FromRow)
import qualified Database.PostgreSQL.Simple as PSQL (query, query_, execute_, execute, executeMany)

import Data.Maybe (listToMaybe)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Int (Int64)

class Monad m => MonadConnPoolReader m where
  askConnPool :: m Connection

liftToMonadConstraints :: (MonadIO m, MonadConnPoolReader m) =>
                          (Connection -> Query -> q -> IO a) -> (Query -> q -> m a)
liftToMonadConstraints f queryText params = do
  conn <- askConnPool
  liftIO $ f conn queryText params

liftToMonadConstraints_ :: (MonadIO m, MonadConnPoolReader m) =>
                          (Connection -> Query -> IO a) -> (Query -> m a)
liftToMonadConstraints_ f queryText = do
  conn <- askConnPool
  liftIO $ f conn queryText

query :: (MonadIO m, MonadConnPoolReader m, ToRow q, FromRow r) =>
         Query -> q -> m [r]
query = liftToMonadConstraints PSQL.query

query_ :: (MonadIO m, MonadConnPoolReader m, FromRow r) =>
          Query -> m [r]
query_ = liftToMonadConstraints_ PSQL.query_

execute :: (MonadIO m, MonadConnPoolReader m, ToRow q) =>
           Query -> q -> m Int64
execute = liftToMonadConstraints PSQL.execute

execute_ :: (MonadIO m, MonadConnPoolReader m) =>
           Query -> m Int64
execute_ = liftToMonadConstraints_ PSQL.execute_

queryMaybe :: (MonadIO m, MonadConnPoolReader m, ToRow q, FromRow r) =>
              Query -> q -> m (Maybe r)
queryMaybe = (fmap listToMaybe .) . query

queryMaybe_ :: (MonadIO m, MonadConnPoolReader m, FromRow r) =>
              Query -> m (Maybe r)
queryMaybe_ = fmap listToMaybe . query_

executeMany :: (MonadIO m, MonadConnPoolReader m, ToRow q) => Query -> [q] -> m Int64
executeMany = liftToMonadConstraints PSQL.executeMany
