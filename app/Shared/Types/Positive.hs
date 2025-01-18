{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Shared.Types.Positive (Positive, mkPositive, unPositive, plus) where

import Servant (FromHttpApiData (parseUrlPiece))
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Data.Aeson (FromJSON, parseJSON, ToJSON)

import Data.Data (Typeable)

import Shared.Types.Internal (mkParseJSON, mkParseUrlPiece, mkFromField)
import Data.String (IsString)

newtype Positive a = Positive a
  deriving (ToJSON, ToField)

mkPositive :: (Num a, Ord a) => a -> Maybe (Positive a)
mkPositive a | a > 0 = Just $ Positive a
             | otherwise = Nothing

unPositive :: Positive a -> a
unPositive (Positive a) = a

infixl 6 `plus`
plus :: Num a => Positive a -> Positive a -> Positive a
Positive x `plus` Positive y = Positive $ x + y

parseErrorMsg :: IsString s => s
parseErrorMsg = "value has to be positive"

instance (FromJSON a, Ord a, Num a) => FromJSON (Positive a) where
  parseJSON = mkParseJSON mkPositive parseErrorMsg
instance (FromHttpApiData a, Ord a, Num a) => FromHttpApiData (Positive a) where
  parseUrlPiece = mkParseUrlPiece mkPositive parseErrorMsg
instance (Typeable a, FromField a, Ord a, Num a) => FromField (Positive a) where
  fromField = mkFromField mkPositive parseErrorMsg
