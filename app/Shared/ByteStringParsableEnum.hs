{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Shared.ByteStringParsableEnum (ByteStringParsableEnum (parseEnumBS)) where

import Data.ByteString (ByteString)

import GHC.Generics
import Control.Applicative ((<|>))
import Data.String (IsString(fromString))

class GParse f where
  gParse :: ByteString -> Maybe (f p)

instance GParse a => GParse (D1 d a) where
  gParse str = M1 <$> gParse str

instance (GParse a, GParse b) => GParse (a :+: b) where
  gParse str
    =   L1 <$> gParse str
    <|> R1 <$> gParse str

instance (Constructor c) => GParse (C1 c U1) where
  gParse str =
    if str == fromString (conName (undefined :: C1 c U1 p))
    then Just (M1 U1)
    else Nothing

instance GParse V1 where
  gParse _ = Nothing

instance GParse U1 where
  gParse _ = Just U1

class ByteStringParsableEnum a where
  parseEnumBS :: ByteString -> Maybe a
  default parseEnumBS :: (Generic a, GParse (Rep a)) => ByteString -> Maybe a
  parseEnumBS str = to <$> gParse str
