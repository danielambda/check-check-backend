{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}

module Infrastructure.Receipts.PGRepository
  ( createReceiptItemsTable
  , ReceiptsRepositoryT(..)
  ) where

import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Data.Text (Text)
import Optics ((^.), toListOf, (%))

import Control.Monad.IO.Class (MonadIO)
import Control.Monad (void)
import GHC.Generics (Generic)
import Data.Maybe (mapMaybe)
import Data.Function ((&))

import SmartPrimitives.Positive (mkPositive)
import Core.Common.Operators ((^^.))
import Core.Receipts.Domain.Receipt (Receipt, receiptItems, mkReceipt)
import Core.Receipts.Domain.ReceiptItem (ReceiptItem(ReceiptItem))
import Core.Receipts.MonadClasses.Repository (ReceiptsRepository (..))
import Infrastructure.Common.Persistence (MonadPG, query, execute_, executeMany)
import Core.Common.Domain.RubKopecks (RubKopecks(..))

newtype ReceiptsRepositoryT m a = ReceiptsRepositoryT
  { runReceiptsRepositoryT :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadPG)

instance MonadPG m => ReceiptsRepository (ReceiptsRepositoryT m) where
  addReceiptToRepo = addReceiptToDb
  getReceiptFromRepo = getReceiptFromDb

addReceiptToDb :: MonadPG m => Text -> Receipt -> m ()
addReceiptToDb qr receipt = void $ executeMany [sql|
  INSERT INTO receiptItems (qr, index, name, price, quantity) VALUES (?, ?, ?, ?, ?)
|] (toDb qr receipt)

getReceiptFromDb :: MonadPG m => Text -> m (Maybe Receipt)
getReceiptFromDb qr = do
  toDomain <$> getReceiptItemsFromDb
  where
    getReceiptItemsFromDb = query [sql|
      SELECT qr, index, name, price, quantity FROM receiptItems WHERE qr = ?
    |] (Only qr)

toDb :: Text -> Receipt -> [DbReceiptItem]
toDb qr receipt =
  let items = receipt & toListOf receiptItems
  in toDb' <$> items
    where
      toDb' (index, item) =
        let (name, price, quantity) = item & (,,) <$> (^. #name) <*> (^^. #price % #value) <*> (^^. #quantity)
        in DbReceiptItem { qr, index, name, price, quantity }

toDomain :: [DbReceiptItem] -> Maybe Receipt
toDomain = mkReceipt . mapMaybe toDomain'
  where
    toDomain' DbReceiptItem{ name, price, quantity } = do
      price' <- mkPositive $ RubKopecks price
      quantity' <- mkPositive quantity
      return $ ReceiptItem name price' quantity'

data DbReceiptItem = DbReceiptItem
  { qr :: Text
  , index :: Int
  , name :: Text
  , price :: Integer
  , quantity :: Double
  } deriving (Generic, FromRow, ToRow)

createReceiptItemsTable :: MonadPG m => m ()
createReceiptItemsTable = void $ execute_ [sql|
  CREATE TABLE IF NOT EXISTS receiptItems
  ( qr TEXT NOT NULL
  , index SMALLINT NOT NULL
  , name TEXT NOT NULL
  , price INTEGER NOT NULL
  , quantity REAL NOT NULL
  )
|]

