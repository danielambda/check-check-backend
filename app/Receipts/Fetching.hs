{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Receipts.Fetching (fetchReceiptItems, Env, mkEnv, MonadEnvReader, askEnv) where

import Network.HTTP.Simple
  ( getResponseBody
  , addRequestHeader
  , setRequestMethod
  , setRequestBodyJSON
  , httpLBS
  , getResponseStatusCode
  , setRequestHost
  , setRequestSecure
  , setRequestPort
  , setRequestPath
  , defaultRequest
  , Request
  )
import Data.ByteString.Char8 (pack)
import Data.Aeson (decode, (.:), object, (.=), FromJSON)
import Data.Aeson.Types (parseMaybe)

import Data.Function ((&))
import Control.Monad.IO.Class (MonadIO)
import System.Environment (getEnv)

import Receipts.Types
import Common.JSON ((*:))

instance FromJSON ReceiptItem

data Env = Env
  { inn :: String
  , password :: String
  , clientSecret :: String
  }

mkEnv :: IO Env
mkEnv = Env
  <$> getEnv "RECEIPTS_INN"
  <*> getEnv "RECEIPTS_PASSWORD"
  <*> getEnv "RECEIPTS_CLIENT_SECRET"

class Monad m => MonadEnvReader m where
  askEnv :: m Env


fetchReceiptItems :: (MonadIO m, MonadEnvReader m) => String -> m [ReceiptItem]
fetchReceiptItems qr =
  getSessionId >>= concatMapM \sessionId ->
    getTicketId sessionId qr >>= concatMapM
      (getReceiptItems sessionId)
  where
    concatMapM :: (Traversable t, Monad m) => (a -> m [b]) -> t a -> m [b]
    concatMapM f xs = fmap concat (mapM f xs)

  -- mSessionId <- getSessionId
  -- case mSessionId of
  --   Just sessionId -> do
  --     mTicketId <- getTicketId sessionId qr
  --     case mTicketId of
  --       Just ticketId -> getReceiptItems sessionId ticketId
  --       Nothing -> return []
  --   Nothing -> return []

getSessionId :: (MonadIO m, MonadEnvReader m) => m (Maybe String)
getSessionId = do
  Env{ inn, password, clientSecret } <- askEnv
  let payload = object
        [ "inn" .= inn
        , "password" .= password
        , "client_secret" .= clientSecret
        ]
  let request = baseRequest
        & setRequestMethod "POST"
        & setRequestPath "/v2/mobile/users/lkfl/auth/"
        & setRequestBodyJSON payload
  response <- httpLBS request
  let mResponseBody = decode $ response & getResponseBody
  let mSessionId = mResponseBody >>= parseMaybe (.: "sessionId")
  return mSessionId

getTicketId :: (MonadIO m, MonadEnvReader m) => String -> String -> m (Maybe String)
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
    _ -> do
      let mResponseBody = decode $ response & getResponseBody
      let mTicketId = mResponseBody >>= parseMaybe (.: "id")
      return mTicketId

getReceiptItems :: MonadIO m => String -> String -> m [ReceiptItem]
getReceiptItems sessionId ticketId = do
  let request = baseRequest
        & setRequestMethod "GET"
        & addRequestHeader "sessionId" (pack sessionId)
        & setRequestPath ("/v2/tickets/" <> pack ticketId)
  response <- httpLBS request
  let mResponseBody = decode $ response & getResponseBody
  let mItems = mResponseBody >>= parseMaybe parseItems
  return $ concat mItems
    where
      parseItems obj = obj .: "ticket" *: "document" *: "receipt" *: "items"

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
