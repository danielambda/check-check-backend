module Main (main) where

import Configuration.Dotenv (loadFile, defaultConfig)
import Data.ByteString.Char8 as B (pack)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Control.Monad.Reader (ReaderT (runReaderT))

import System.Environment (getEnv)

import Infrastructure.Common.Persistence (MonadPG, withTransaction)
import Infrastructure.Receipts.PGRepository (createReceiptItemsTable)
import Infrastructure.Users.PGRepository (createUsersTable, createBudgetsTable, createOtherUserIdsTable, createUserContactsTable)
import Infrastructure.Users.Requests.PGRepository (createRequestsTable, createRequestItemsTable)

main :: IO ()
main = do
  loadFile defaultConfig
  getEnv "POSTGRESQL_CONNECTION_STRING"
    >>= connectPostgreSQL . B.pack
    >>= runReaderT initDb

initDb :: MonadPG m => m ()
initDb = withTransaction $ do
  createReceiptItemsTable
  createUsersTable
  createBudgetsTable
  createOtherUserIdsTable
  createUserContactsTable
  createRequestsTable
  createRequestItemsTable
