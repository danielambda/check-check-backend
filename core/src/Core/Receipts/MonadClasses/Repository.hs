module Core.Receipts.MonadClasses.Repository (ReceiptsRepository(..)) where

import Data.Text (Text)

import Core.Receipts.Domain.Receipt (Receipt)

class Monad m => ReceiptsRepository m where
  getReceiptFromRepo :: Text -> m (Maybe Receipt)
  addReceiptToRepo :: Text -> Receipt -> m ()

