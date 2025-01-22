{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SmartPrimitives.Positive (Positive, mkPositive, unPositive, plus, mult) where

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

newtype Positive a = Positive a
  deriving (ToJSON, ToField)

mkPositive :: (Num a, Ord a) => a -> Maybe (Positive a)
mkPositive a | a > 0 = Just $ Positive a
             | otherwise = Nothing

unPositive :: Positive a -> a
unPositive (Positive a) = a

instance LabelOptic "value" A_Getter (Positive a) (Positive a) a a where
  labelOptic = to unPositive

infixl 6 `plus`
plus :: Num a => Positive a -> Positive a -> Positive a
Positive x `plus` Positive y = Positive $ x + y

infixl 7 `mult`
mult :: Num a => Positive a -> Positive a -> Positive a
Positive x `mult` Positive y = Positive $ x * y

parseErrorMsg :: IsString s => s
parseErrorMsg = "value has to be positive"

instance (FromJSON a, Ord a, Num a) => FromJSON (Positive a) where
  parseJSON = mkParseJSON mkPositive parseErrorMsg
instance (FromHttpApiData a, Ord a, Num a) => FromHttpApiData (Positive a) where
  parseUrlPiece = mkParseUrlPiece mkPositive parseErrorMsg
instance (Typeable a, FromField a, Ord a, Num a) => FromField (Positive a) where
  fromField = mkFromField mkPositive parseErrorMsg
