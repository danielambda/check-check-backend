{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Shared.Types.TextMinLen (TextMinLen, mkTextMinLen, unTextMinLen) where

import Servant (parseUrlPiece, FromHttpApiData)
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Data.Aeson (FromJSON, parseJSON)
import Data.Text (Text)
import qualified Data.Text as T

import GHC.TypeLits (Nat, KnownNat, natVal)
import Data.Data (Proxy (Proxy))
import Data.String (IsString (fromString))

import Shared.Types.Internal (mkFromField, mkParseJSON, mkParseUrlPiece)

newtype TextMinLen (n :: Nat) = TextMinLen Text

mkTextMinLen :: forall n. (KnownNat n) => Text -> Maybe (TextMinLen n)
mkTextMinLen text
  | T.length text <= minLen = Just (TextMinLen text)
  | otherwise = Nothing
  where
    minLen = fromInteger $ natVal (Proxy @n)

unTextMinLen :: TextMinLen n -> Text
unTextMinLen (TextMinLen text) = text

parseErrorMsg :: (KnownNat n, Semigroup s, IsString s) => Proxy n -> s
parseErrorMsg proxy = "text length has to be at least " <> fromString (show $ natVal proxy)

instance (KnownNat n) => FromHttpApiData (TextMinLen n) where
  parseUrlPiece = mkParseUrlPiece mkTextMinLen $ parseErrorMsg (Proxy @n)
instance (KnownNat n) => FromField (TextMinLen n) where
  fromField = mkFromField mkTextMinLen $ parseErrorMsg (Proxy @n)
instance (KnownNat n) => FromJSON (TextMinLen n) where
  parseJSON = mkParseJSON mkTextMinLen $ parseErrorMsg (Proxy @n)
