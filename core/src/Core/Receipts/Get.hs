{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ConstraintKinds #-}

module Core.Receipts.Get (Dependencies, get) where

import Data.Maybe (mapMaybe)
import Data.Foldable (traverse_)

import Data.Text (Text)

import SmartPrimitives.Positive (mkPositive)
import Core.Common.Operators ((.>), (*>>))
import Core.Common.Domain.RubKopecks (RubKopecks(..))
import Core.Receipts.MonadClasses.Fetching (ReceiptsFetching(..), FetchedReceiptItem(..))
import Core.Receipts.MonadClasses.Repository (ReceiptsRepository(..))
import Core.Receipts.Domain.Receipt (Receipt, mkReceipt)
import Core.Receipts.Domain.ReceiptItem (ReceiptItem(ReceiptItem))

type Dependencies m = (ReceiptsRepository m, ReceiptsFetching m)
get :: Dependencies m => Text -> m (Maybe Receipt)
get qr =
  getReceiptFromRepo qr >>=
    maybe fetchAndStoreToRepo (return . Just)
  where
    fetchAndStoreToRepo =
      fetchReceiptItems qr
      >>= fetchedToDomain
      .> (traverse_ (addReceiptToRepo qr) *>> return)

fetchedToDomain :: [FetchedReceiptItem] -> Maybe Receipt
fetchedToDomain = mkReceipt . mapMaybe mapItem
  where
    mapItem FetchedReceiptItem{ name, price, quantity } = do
      posPrice <- mkPositive $ RubKopecks price
      posQuantity <- mkPositive quantity
      return $ ReceiptItem name posPrice posQuantity
