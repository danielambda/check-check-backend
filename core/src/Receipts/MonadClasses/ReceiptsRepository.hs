module Receipts.MonadClasses.ReceiptsRepository
  ( ReceiptsRepository(..)
  ) where

import Receipts.Domain.Receipt (Receipt)

class Monad m => ReceiptsRepository m where
  getReceiptFromRepo :: String -> m (Maybe Receipt)
  addReceiptToRepo :: String -> Receipt -> m ()

