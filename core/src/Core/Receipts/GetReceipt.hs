{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ConstraintKinds #-}

module Core.Receipts.GetReceipt (getReceipt, Dependencies) where

import Data.Maybe (mapMaybe)
import Data.Foldable (traverse_)
import Data.Function ((&))

import SmartPrimitives.Positive (mkPositive)
import Core.Receipts.MonadClasses.Fetching (ReceiptsFetching(..), FetchedReceiptItem(..))
import Core.Receipts.MonadClasses.Repository (ReceiptsRepository(..))
import Core.Receipts.Domain.Receipt (Receipt, mkReceipt)
import Core.Receipts.Domain.ReceiptItem (mkReceiptItem)

type Dependencies m = (ReceiptsRepository m, ReceiptsFetching m)
getReceipt :: Dependencies m => String -> m (Maybe Receipt)
getReceipt qr =
  getReceiptFromRepo qr >>=
    maybe fetchAndStoreToRepo (return . Just)
  where
    fetchAndStoreToRepo =
      fetchReceiptItems qr >>= \fetchedItems ->
      fetchedItems
      & fetchedToDomain
      & traverse_ (addReceiptToRepo qr)
      *>> return

(*>>) :: (Monad m, Applicative f) => f (m a) -> f (m b) -> f (m b)
(*>>) = liftA2 (>>)

fetchedToDomain :: [FetchedReceiptItem] -> Maybe Receipt
fetchedToDomain = mkReceipt . mapMaybe mapItem
  where
    mapItem FetchedReceiptItem{ name, price, quantity } = do
      posPrice <- mkPositive price
      posQuantity <- mkPositive quantity
      return $ mkReceiptItem name posPrice posQuantity
