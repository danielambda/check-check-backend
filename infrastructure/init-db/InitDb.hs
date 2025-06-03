module Main (main) where

import Data.ByteString.Char8 as B (pack)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Control.Monad.Reader (ReaderT (runReaderT))

import System.Environment (getEnv)

import Infrastructure.Common.Persistence (MonadPG, withTransaction)

main :: IO ()
main = getEnv "POSTGRESQL_CONNECTION_STRING"
  >>= connectPostgreSQL . B.pack
  >>= runReaderT initDb

initDb :: MonadPG m => m ()
initDb = withTransaction $ do
  pure ()
