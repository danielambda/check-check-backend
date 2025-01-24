{-# LANGUAGE FlexibleInstances #-}

module Core.Receipts.MonadClasses.Fetching
  ( ReceiptsFetching(..)
  , FetchedReceiptItem(..)
  ) where

import Data.Text (Text)

import Control.Monad.Trans (MonadTrans (lift))

class Monad m => ReceiptsFetching m where
  fetchReceiptItems :: String -> m [FetchedReceiptItem]

data FetchedReceiptItem = FetchedReceiptItem
  { name :: Text
  , price :: Integer
  , quantity :: Double
  }

