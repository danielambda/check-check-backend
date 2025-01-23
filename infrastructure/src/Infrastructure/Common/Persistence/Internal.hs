module Infrastructure.Common.Persistence.Internal
  ( queryMaybe
  , querySingleField
  ) where

import Database.PostgreSQL.Simple (Query, ToRow, FromRow , fromOnly)
import Database.PostgreSQL.Simple.FromField (FromField)

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (listToMaybe)

import Infrastructure.Common.Persistence (MonadConnReader, query)

querySingleField :: (MonadIO m, MonadConnReader m, ToRow q, FromField f)
                 => Query -> q -> m f
querySingleField = (fmap (fromOnly . head) .) . query

queryMaybe :: (MonadIO m, MonadConnReader m, ToRow q, FromRow r)
           => Query -> q -> m (Maybe r)
queryMaybe = (fmap listToMaybe .) . query

