{-# LANGUAGE FlexibleInstances #-}

module Core.Receipts.MonadClasses.Fetching
  ( ReceiptsFetching(..)
  , FetchedReceiptItem(..)
  ) where

import Data.Text (Text)

class Monad m => ReceiptsFetching m where
  fetchReceiptItems :: Text -> m [FetchedReceiptItem]

data FetchedReceiptItem = FetchedReceiptItem
  { name :: Text
  , price :: Integer
  , quantity :: Double
  }

