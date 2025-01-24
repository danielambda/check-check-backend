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
  ( getReceiptFromDb, addReceiptToDb
  , createReceiptItemsTable
  , ReceiptsRepositoryT(..)
  ) where

import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Data.Text (Text)
import Optics ((^.), (%), toListOf)

import Control.Monad.IO.Class (MonadIO)
import Control.Monad (void)
import GHC.Generics (Generic)
import Data.Maybe (mapMaybe)
import Data.Function ((&))
import Data.Functor ((<&>))

import Optics.Tuppled (tuppledFields3)
import SmartPrimitives.Positive (mkPositive)
import Core.Receipts.Domain.Receipt (Receipt, receiptItems, mkReceipt)
import Core.Receipts.Domain.ReceiptItem (mkReceiptItem)
import Core.Receipts.MonadClasses.Repository (ReceiptsRepository (..))
import Infrastructure.Common.Persistence (MonadConnReader, query, execute_, executeMany)

import Control.Monad.Trans (MonadTrans, lift)

newtype ReceiptsRepositoryT m a = ReceiptsRepositoryT
  { runReceiptsRepositoryT :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadConnReader)

instance MonadTrans ReceiptsRepositoryT where
  lift = ReceiptsRepositoryT
instance (MonadTrans t, MonadIO m, MonadConnReader m) => ReceiptsRepository (t (ReceiptsRepositoryT m)) where
  getReceiptFromRepo = lift . getReceiptFromRepo
  addReceiptToRepo = (lift .) . addReceiptToRepo

instance (MonadIO m, MonadConnReader m) => ReceiptsRepository (ReceiptsRepositoryT m) where
  getReceiptFromRepo = getReceiptFromDb
  addReceiptToRepo = addReceiptToDb

getReceiptFromDb :: (MonadIO m, MonadConnReader m)
                 => String -> m (Maybe Receipt)
getReceiptFromDb qr = do
  getReceiptItemsFromDb qr
  <&> toDomain

toDomain :: [DbReceiptItem] -> Maybe Receipt
toDomain = mkReceipt . mapMaybe toDomain'
  where
    toDomain' DbReceiptItem{ name, price, quantity } = do
      price' <- mkPositive price
      quantity' <- mkPositive quantity
      return $ mkReceiptItem name price' quantity'

getReceiptItemsFromDb :: (MonadIO m, MonadConnReader m)
                      => String -> m [DbReceiptItem]
getReceiptItemsFromDb qr = query [sql|
  SELECT qr, index, name, price, quantity FROM receipt_items WHERE qr = ?
|] (Only qr)

addReceiptToDb :: (MonadIO m, MonadConnReader m)
               => String -> Receipt -> m ()
addReceiptToDb qr receipt = void $ executeMany [sql|
  INSERT INTO receipt_items (name, price, quantity, index, qr) VALUES (?, ?, ?, ?, ?)
|] (toDb qr receipt)

toDb :: String -> Receipt -> [DbReceiptItem]
toDb qr receipt =
  let items = receipt & toListOf receiptItems
  in toDb' <$> items
    where
      toDb' (index, item) =
        let (name, price, quantity) = item ^. tuppledFields3 #name (#price % #value) (#quantity % #value)
        in DbReceiptItem { qr, index, name, price, quantity }

data DbReceiptItem = DbReceiptItem
  { qr :: String
  , index :: Int
  , name :: Text
  , price :: Integer
  , quantity :: Double
  } deriving (Generic, FromRow, ToRow)

createReceiptItemsTable :: (MonadIO m, MonadConnReader m) => m ()
createReceiptItemsTable = void $ execute_ [sql|
  CREATE TABLE IF NOT EXISTS receipt_items
  ( qr TEXT NOT NULL
  , index SMALLINT NOT NULL
  , name TEXT NOT NULL
  , price INTEGER NOT NULL
  , quantity REAL NOT NULL
  )
|]

