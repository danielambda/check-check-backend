{-# LANGUAGE QuasiQuotes #-}

module Users.Persistence (createUsersTable) where

import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Database.PostgreSQL.Simple as PG

import Control.Monad (void)

import Database.PostgreSQL.Simple (Connection)

createUsersTable :: Connection -> IO ()
createUsersTable conn = void $ PG.execute_ conn [sql|
  CREATE TABLE users
  ( id UUID NOT NULL
  , username TEXT NOT NULL
  , group_id UUID REFERENCES groups(id)
  , created_at TIMESTAMP DEFAULT NOW()
  , PRIMARY KEY (id)
  )
|]
