module CustomUtils.Persistence (queryMaybe) where

import Database.PostgreSQL.Simple (Query, Connection, ToRow, FromRow, query)
import Data.Maybe (listToMaybe)

queryMaybe :: (ToRow q, FromRow r) => Connection -> Query -> q -> IO (Maybe r)
queryMaybe = fmap listToMaybe ... query
  where (...) = (.) . (.) . (.)
