module Receipts.MonadClasses.ReceiptsFetching
  ( ReceiptsFetching(..)
  , FetchedReceiptItem(..)
  ) where

import Data.Text (Text)

class Monad m => ReceiptsFetching m where
  fetchReceiptItems :: String -> m [FetchedReceiptItem]

data FetchedReceiptItem = FetchedReceiptItem
  { name :: Text
  , price :: Integer
  , quantity :: Double
  }

