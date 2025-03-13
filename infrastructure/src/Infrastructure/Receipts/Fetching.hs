{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Infrastructure.Receipts.Fetching (ReceiptsFetchingT(..)) where

import Network.HTTP.Simple
  ( getResponseBody, addRequestHeader, setRequestMethod, setRequestBodyJSON
  , httpLBS, getResponseStatusCode, setRequestHost, setRequestSecure
  , setRequestPort, setRequestPath, defaultRequest, Request
  )
import Data.ByteString.Char8 (pack)
import Data.Aeson (decode, (.:), object, (.=), FromJSON)
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text)

import Data.Function ((&))
import GHC.Generics (Generic)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import System.Environment (getEnv)

import Core.Receipts.MonadClasses.Fetching
  ( ReceiptsFetching(fetchReceiptItems)
  , FetchedReceiptItem(FetchedReceiptItem, name, quantity, price)
  )

newtype ReceiptsFetchingT m a = ReceiptsFetchingT
  { runReceiptsFetchingT :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance MonadIO m => ReceiptsFetching (ReceiptsFetchingT m) where
  fetchReceiptItems qr = do
    env <- mkEnv
    map fromDto <$> fetchReceiptItems' qr `runReaderT` env
    where
      fromDto FetchedReceiptItemDto{..} = FetchedReceiptItem{..}

      mkEnv = liftIO $ Env
        <$> getEnv "RECEIPTS_INN"
        <*> getEnv "RECEIPTS_PASSWORD"
        <*> getEnv "RECEIPTS_CLIENT_SECRET"

data Env = Env
  { inn :: String
  , password :: String
  , clientSecret :: String
  }

data FetchedReceiptItemDto = FetchedReceiptItemDto
  { name :: Text
  , price :: Integer
  , quantity :: Double
  } deriving (Generic, FromJSON)

fetchReceiptItems' :: (MonadIO m, MonadReader Env m) => Text -> m [FetchedReceiptItemDto]
fetchReceiptItems' qr =
  getSessionId >>= concatMapM \sessionId ->
    getTicketId sessionId qr >>= concatMapM
      (getReceiptItems sessionId)
  where
    concatMapM :: (Traversable t, Monad m) => (a -> m [b]) -> t a -> m [b]
    concatMapM f xs = concat <$> mapM f xs

getSessionId :: (MonadIO m, MonadReader Env m) => m (Maybe String)
getSessionId = do
  Env{ inn, password, clientSecret } <- ask
  let payload = object
        [ "inn" .= inn
        , "password" .= password
        , "client_secret" .= clientSecret
        ]
  let request = baseRequest
        & setRequestMethod "POST"
        & setRequestPath "/v2/mobile/users/lkfl/auth/"
        & setRequestBodyJSON payload
  httpLBS request >>= \response -> response
    & getResponseBody
    & decode
    >>= parseMaybe (.: "sessionId")
    & return

getTicketId :: (MonadIO m, MonadReader Env m) => String -> Text -> m (Maybe String)
getTicketId sessionId qr = do
  let request = baseRequest
        & setRequestMethod "POST"
        & addRequestHeader "sessionId" (pack sessionId)
        & setRequestPath "/v2/ticket/"
        & setRequestBodyJSON (object ["qr" .= qr])
  response <- httpLBS request
  let statusCode = response & getResponseStatusCode
  case statusCode of
    429 -> do -- Too Many Requests
      return Nothing
    401 -> do -- Unauthorized
      mNewSessionId <- getSessionId
      case mNewSessionId of
        Nothing -> return Nothing
        Just newSessionId -> getTicketId newSessionId qr
    _ -> response
      & getResponseBody
      & decode
      >>= parseMaybe (.: "id")
      & return

getReceiptItems :: MonadIO m => String -> String -> m [FetchedReceiptItemDto]
getReceiptItems sessionId ticketId =
  httpLBS request >>= \response ->
    response
    & getResponseBody
    & decode
    >>= parseMaybe parseItems
    & concat
    & return
  where
    request = baseRequest
      & setRequestMethod "GET"
      & addRequestHeader "sessionId" (pack sessionId)
      & setRequestPath ("/v2/tickets/" <> pack ticketId)

    parseItems obj = obj .: "ticket" *: "document" *: "receipt" *: "items"

    pObj *: key = pObj >>= (.: key)

baseRequest :: Request
baseRequest = defaultRequest
  & setRequestHost "irkkt-mobile.nalog.ru"
  & setRequestSecure True
  & setRequestPort 8888
  & addRequestHeader "Accept"          "*/*"
  & addRequestHeader "Device-OS"       "IOS"
  & addRequestHeader "Device-Id"       "7C82010F-16CC-446B-8F66-FC4080C66521"
  & addRequestHeader "clientVersion"   "2.9.0"
  & addRequestHeader "Accept-Language" "ru-RU;q=1, en-US;q=0.9"
  & addRequestHeader "User-Agent"      "billchecker/2.9.0 (iPhone; iOS 13.6; Scale/2.00)"
