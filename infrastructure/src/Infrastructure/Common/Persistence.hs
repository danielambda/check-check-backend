{-# LANGUAGE FlexibleInstances #-}

module Infrastructure.Common.Persistence
  ( MonadConnReader, askConn
  , query, queryMaybe, query_
  , execute, execute_
  , executeMany
  , withTransaction
  ) where

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Database.PostgreSQL.Simple (Query, Connection, ToRow, FromRow)
import qualified Database.PostgreSQL.Simple as PG
  ( query, query_, execute_
  , execute, executeMany
  , withTransaction
  )

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Int (Int64)
import Data.Maybe (listToMaybe)

class Monad m => MonadConnReader m where
  askConn :: m Connection

instance Monad m => MonadConnReader (ReaderT Connection m) where
  askConn = ask

query :: (MonadIO m, MonadConnReader m, ToRow q, FromRow r)
      => Query -> q -> m [r]
query = liftToMonadConstraints PG.query

queryMaybe :: (MonadIO m, MonadConnReader m, ToRow q, FromRow r)
           => Query -> q -> m (Maybe r)
queryMaybe queryText q = listToMaybe <$> query queryText q

query_ :: (MonadIO m, MonadConnReader m, FromRow r)
       => Query -> m [r]
query_ = liftToMonadConstraints_ PG.query_

execute :: (MonadIO m, MonadConnReader m, ToRow q)
        => Query -> q -> m Int64
execute = liftToMonadConstraints PG.execute

execute_ :: (MonadIO m, MonadConnReader m)
         => Query -> m Int64
execute_ = liftToMonadConstraints_ PG.execute_

executeMany :: (MonadIO m, MonadConnReader m, ToRow q) => Query -> [q] -> m Int64
executeMany = liftToMonadConstraints PG.executeMany

withTransaction :: (MonadIO m, MonadConnReader m) => ReaderT Connection IO a -> m a
withTransaction actions = do
  conn <- askConn
  let actionsIO = actions `runReaderT` conn
  liftIO $ PG.withTransaction conn actionsIO

liftToMonadConstraints :: (MonadIO m, MonadConnReader m)
                       => (Connection -> Query -> q -> IO a) -> (Query -> q -> m a)
liftToMonadConstraints f queryText params = do
  conn <- askConn
  liftIO $ f conn queryText params

liftToMonadConstraints_ :: (MonadIO m, MonadConnReader m)
                        => (Connection -> Query -> IO a) -> (Query -> m a)
liftToMonadConstraints_ f queryText = do
  conn <- askConn
  liftIO $ f conn queryText

