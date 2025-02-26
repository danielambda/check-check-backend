{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module SmartPrimitives.Negative
  ( Negative, mkNegative, unNegative
  , mkUnsafeNegative
  , plus
  , sumNegative
  ) where

import Servant (FromHttpApiData (parseUrlPiece))
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Data.Aeson (FromJSON, parseJSON, ToJSON)
import Optics (A_Getter)
import Optics.Label (LabelOptic (labelOptic))
import Optics.Getter (to)

import Data.Data (Typeable)
import Data.String (IsString)
import Data.List.NonEmpty (NonEmpty)

import SmartPrimitives.Internal (mkParseJSON, mkParseUrlPiece, mkFromField)

newtype Negative a = Negative a
  deriving (Eq, Ord, Show, ToJSON, ToField)

mkNegative :: (Num a, Ord a) => a -> Maybe (Negative a)
mkNegative a | a < 0 = Just $ Negative a
             | otherwise = Nothing

mkUnsafeNegative :: (Num a, Ord a) => a -> Negative a
mkUnsafeNegative = Negative

unNegative :: Negative a -> a
unNegative (Negative a) = a

instance LabelOptic "value" A_Getter (Negative a) (Negative a) a a where
  labelOptic = to unNegative

infixl 6 `plus`
plus :: Num a => Negative a -> Negative a -> Negative a
Negative x `plus` Negative y = Negative $ x + y

sumNegative :: Num a => NonEmpty (Negative a) -> Negative a
sumNegative = Negative . sum . fmap unNegative

parseErrorMsg :: IsString s => s
parseErrorMsg = "value has to be negative"

instance (FromJSON a, Ord a, Num a) => FromJSON (Negative a) where
  parseJSON = mkParseJSON mkNegative parseErrorMsg
instance (FromHttpApiData a, Ord a, Num a) => FromHttpApiData (Negative a) where
  parseUrlPiece = mkParseUrlPiece mkNegative parseErrorMsg
instance (Typeable a, FromField a, Ord a, Num a) => FromField (Negative a) where
  fromField = mkFromField mkNegative parseErrorMsg
