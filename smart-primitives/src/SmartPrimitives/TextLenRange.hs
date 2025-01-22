{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module SmartPrimitives.TextLenRange (TextLenRange, mkTextLenRange, unTextLenRange) where

import Data.Text (Text)
import qualified Data.Text as T

import GHC.TypeLits (Nat, KnownNat, natVal)
import Data.Data (Proxy (Proxy))
import Data.String (IsString (fromString))
import Servant (FromHttpApiData (parseUrlPiece))
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Data.Aeson (FromJSON (parseJSON))

import SmartPrimitives.Internal (mkFromFieldEither, mkParseUrlPieceEither, mkParseJSONEither)

newtype TextLenRange (min :: Nat) (max :: Nat) = TextLenRange Text

data TextLenRangeError
  = TextLenTooShort Int -- Expected min length
  | TextLenTooLong Int  -- Expected max length
  deriving (Show, Eq)

mkTextLenRange :: forall min max. (KnownNat min, KnownNat max)
               => Text -> Either TextLenRangeError (TextLenRange min max)
mkTextLenRange text
  | len < minLen = Left $ TextLenTooShort minLen
  | len > maxLen = Left $ TextLenTooLong minLen
  | otherwise = Right $ TextLenRange text
  where
    len = T.length text
    minLen = fromInteger $ natVal (Proxy @min)
    maxLen = fromInteger $ natVal (Proxy @max)

unTextLenRange :: TextLenRange min max -> Text
unTextLenRange (TextLenRange text) = text

parseErrorMsg :: (Semigroup s, IsString s) => TextLenRangeError -> s
parseErrorMsg (TextLenTooShort n) = "text length has to be as least " <> fromString (show n)
parseErrorMsg (TextLenTooLong n) = "text length has to be as least " <> fromString (show n)

instance (KnownNat min, KnownNat max) => FromHttpApiData (TextLenRange min max) where
  parseUrlPiece = mkParseUrlPieceEither mkTextLenRange parseErrorMsg
instance (KnownNat min, KnownNat max) => FromField (TextLenRange min max) where
  fromField = mkFromFieldEither mkTextLenRange parseErrorMsg
instance (KnownNat min, KnownNat max) => FromJSON (TextLenRange min max) where
  parseJSON = mkParseJSONEither mkTextLenRange parseErrorMsg
