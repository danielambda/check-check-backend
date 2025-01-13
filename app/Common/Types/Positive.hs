{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Common.Types.Positive (Positive, mkPositive, unPositive) where

import Data.Aeson (FromJSON, parseJSON, ToJSON)
import Data.Aeson.Types (parseFail)
import Servant (FromHttpApiData (parseUrlPiece))
import Database.PostgreSQL.Simple.ToField (ToField)

newtype Positive a = Positive { unPositive' :: a }
  deriving (ToJSON, ToField)

mkPositive :: (Num a, Ord a) => a -> Maybe (Positive a)
mkPositive a | a > 0 = Just $ Positive a
             | otherwise = Nothing

unPositive :: Positive a -> a
unPositive = unPositive'

instance (FromJSON a, Ord a, Num a) => FromJSON (Positive a) where
  parseJSON value = do
    a <- mkPositive <$> parseJSON value
    maybe (parseFail "value has to be positive") return a

instance (FromHttpApiData a, Ord a, Num a) => FromHttpApiData (Positive a) where
  parseUrlPiece value = do
    a <- mkPositive <$> parseUrlPiece value
    maybe (Left "value has to be positive") return a

