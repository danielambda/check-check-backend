{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SmartPrimitives.TextMaxLen (TextMaxLen, mkTextMaxLen, unTextMaxLen) where

import Servant (FromHttpApiData, parseUrlPiece)
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Data.Aeson (FromJSON (parseJSON), ToJSON)
import Data.Text (Text)
import qualified Data.Text as T

import GHC.TypeLits (Nat, KnownNat, natVal)
import Data.Data (Proxy (Proxy))
import Data.String (IsString (fromString))

import SmartPrimitives.Internal (mkParseJSON, mkParseUrlPiece, mkFromField)
import Database.PostgreSQL.Simple.ToField (ToField)

newtype TextMaxLen (n :: Nat) = TextMaxLen Text
  deriving (ToField, ToJSON)

mkTextMaxLen :: forall n. (KnownNat n) => Text -> Maybe (TextMaxLen n)
mkTextMaxLen text
  | T.length text <= maxLen = Just (TextMaxLen text)
  | otherwise = Nothing
  where
    maxLen = fromInteger $ natVal (Proxy @n)

unTextMaxLen :: TextMaxLen n -> Text
unTextMaxLen (TextMaxLen text) = text

parseErrorMsg :: (KnownNat n, Semigroup s, IsString s) => Proxy n -> s
parseErrorMsg proxy = "text length has to be at most " <> fromString (show $ natVal proxy)

instance (KnownNat n) => FromHttpApiData (TextMaxLen n) where
  parseUrlPiece = mkParseUrlPiece mkTextMaxLen $ parseErrorMsg (Proxy @n)
instance (KnownNat n) => FromField (TextMaxLen n) where
  fromField = mkFromField mkTextMaxLen $ parseErrorMsg (Proxy @n)
instance (KnownNat n) => FromJSON (TextMaxLen n) where
  parseJSON = mkParseJSON mkTextMaxLen $ parseErrorMsg (Proxy @n)
