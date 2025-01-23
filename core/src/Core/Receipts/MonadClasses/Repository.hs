module Core.Receipts.MonadClasses.Repository (ReceiptsRepository(..)) where

import Core.Receipts.Domain.Receipt (Receipt)

class Monad m => ReceiptsRepository m where
  getReceiptFromRepo :: String -> m (Maybe Receipt)
  addReceiptToRepo :: String -> Receipt -> m ()

