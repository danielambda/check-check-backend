{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Common.Persistence.Internal.ByteStringParsableEnum
  ( ByteStringParsableEnum(..)
  , mkEnumFieldParser
  ) where

import Database.PostgreSQL.Simple.FromField (FieldParser, typename, returnError, ResultError (..))
import Data.ByteString (ByteString)

import Control.Applicative ((<|>))
import Data.String (IsString(fromString))
import Data.Typeable (Typeable)
import GHC.Generics (Generic(..), Constructor(..), V1, U1(..), M1(..), type (:+:)(..), D1, C1)

class ByteStringParsableEnum a where
  parseEnumBS :: ByteString -> Maybe a
  default parseEnumBS :: (Generic a, GParse (Rep a)) => ByteString -> Maybe a
  parseEnumBS str = to <$> gParse str

mkEnumFieldParser :: (Typeable a, ByteStringParsableEnum a) => ByteString -> FieldParser a
mkEnumFieldParser enumName field mdata = do
  n <- typename field
  if n /= enumName
  then returnError Incompatible field $ show $ "Unexpected field type "<>n<>", expected "<>enumName
  else case mdata of
    Nothing -> returnError UnexpectedNull field ""
    Just bs -> case parseEnumBS bs of
      Nothing -> returnError ConversionFailed field (show bs)
      Just x  -> return x

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

