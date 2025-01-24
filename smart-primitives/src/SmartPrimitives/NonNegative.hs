{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SmartPrimitives.NonNegative (NonNegative, mkNonNegative, unNonNegative, plus, mult) where

import Servant (FromHttpApiData (parseUrlPiece))
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Data.Aeson (FromJSON, parseJSON, ToJSON)
import Optics (A_Getter)
import Optics.Label (LabelOptic (labelOptic))
import Optics.Getter (to)

import Data.Data (Typeable)
import Data.String (IsString)

import SmartPrimitives.Internal (mkParseJSON, mkParseUrlPiece, mkFromField)

newtype NonNegative a = NonNegative a
  deriving (ToJSON, ToField)

mkNonNegative :: (Num a, Ord a) => a -> Maybe (NonNegative a)
mkNonNegative a | a >= 0 = Just $ NonNegative a
                | otherwise = Nothing

unNonNegative :: NonNegative a -> a
unNonNegative (NonNegative a) = a

instance LabelOptic "value" A_Getter (NonNegative a) (NonNegative a) a a where
  labelOptic = to unNonNegative

infixl 6 `plus`
plus :: Num a => NonNegative a -> NonNegative a -> NonNegative a
NonNegative x `plus` NonNegative y = NonNegative $ x + y

infixl 7 `mult`
mult :: Num a => NonNegative a -> NonNegative a -> NonNegative a
NonNegative x `mult` NonNegative y = NonNegative $ x * y

parseErrorMsg :: IsString s => s
parseErrorMsg = "value has to be non negative"

instance (FromJSON a, Ord a, Num a) => FromJSON (NonNegative a) where
  parseJSON = mkParseJSON mkNonNegative parseErrorMsg
instance (FromHttpApiData a, Ord a, Num a) => FromHttpApiData (NonNegative a) where
  parseUrlPiece = mkParseUrlPiece mkNonNegative parseErrorMsg
instance (Typeable a, FromField a, Ord a, Num a) => FromField (NonNegative a) where
  fromField = mkFromField mkNonNegative parseErrorMsg
