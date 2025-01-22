module SmartPrimitives.Internal
  ( mkParseJSON
  , mkParseUrlPiece
  , mkFromField
  , mkParseJSONEither
  , mkParseUrlPieceEither
  , mkFromFieldEither
  ) where

import Servant (FromHttpApiData (parseUrlPiece))
import Data.Aeson (FromJSON (parseJSON))
import Data.Aeson.Types (Value, Parser, parseFail)
import Data.Text (Text)

import Data.ByteString (ByteString)
import Data.Data (Typeable)
import Database.PostgreSQL.Simple.FromField (FromField (fromField), Field, Conversion, returnError, ResultError (ConversionFailed))

mkParseJSON :: FromJSON b => (b -> Maybe a) -> String -> (Value -> Parser a)
mkParseJSON mk errorMsg value = do
  a <- mk <$> parseJSON value
  maybe (parseFail errorMsg) return a

mkParseUrlPiece :: FromHttpApiData b => (b -> Maybe a) -> Text -> (Text -> Either Text a)
mkParseUrlPiece mk errorMsg value = do
  a <- mk <$> parseUrlPiece value
  maybe (Left errorMsg) return a

mkFromField :: (Typeable a, FromField b)
            => (b -> Maybe a) -> String -> (Field -> Maybe ByteString -> Conversion a)
mkFromField mk errorMsg field mdata = do
  a <- mk <$> fromField field mdata
  maybe (returnError ConversionFailed field errorMsg) return a

mkParseJSONEither :: FromJSON b => (b -> Either e a) -> (e -> String) -> (Value -> Parser a)
mkParseJSONEither mk errorToString value = do
  a <- mk <$> parseJSON value
  either (parseFail . errorToString) return a

mkParseUrlPieceEither :: FromHttpApiData b
                => (b -> Either e a) -> (e -> Text) -> (Text -> Either Text a)
mkParseUrlPieceEither mk errorToString value = do
  a <- mk <$> parseUrlPiece value
  either (Left . errorToString) return a

mkFromFieldEither :: (Typeable a, FromField b)
                  => (b -> Either e a) -> (e -> String)
                  -> (Field -> Maybe ByteString -> Conversion a)
mkFromFieldEither mk errorToString field mdata = do
  a <- mk <$> fromField field mdata
  either (returnError ConversionFailed field . errorToString) return a
