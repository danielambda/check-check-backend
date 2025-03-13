{-# OPTIONS_GHC -Wno-partial-fields #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CheckCheck.Contracts.Users.IncomingRequests
  ( IncomingRequestsAPI
  , GetIncomingRequests
  , CompleteIncomingRequest
  , RequestResp(..)
  , RequestItemResp(..)
  , CompleteIncomingRequestReqBody(..)
  , RoundingStrategyReqBody(..)
  , CompleteIncomingRequestResp(..)
  ) where

import Servant.API ((:<|>), Get, JSON, Capture, ReqBody, (:>), Put)
import Data.UUID (UUID)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import SmartPrimitives.Positive (Positive)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON (toJSON), FromJSON (parseJSON), withObject, (.:), (.:?), withText, KeyValue ((.=)), object, Value (String))
import Data.Time (UTCTime)
import CheckCheck.Contracts.Users.Budget (BudgetResp)

type IncomingRequestsAPI
  =    GetIncomingRequests
  :<|> CompleteIncomingRequest

type GetIncomingRequests =
  Get '[JSON] [RequestResp]

data RequestResp = RequestResp
  { requestId :: UUID
  , senderId :: UUID
  , recipientId :: UUID
  , items :: NonEmpty RequestItemResp
  , createdAt :: UTCTime
  , isPending :: Bool
  } deriving (Generic, ToJSON, FromJSON)

data RequestItemResp = RequestItemResp
  { identity :: Text
  , quantity :: Positive Double
  , price :: Positive Integer
  } deriving (Generic, ToJSON, FromJSON)

type CompleteIncomingRequest
  =  Capture "requestId" UUID
  :> ReqBody '[JSON] CompleteIncomingRequestReqBody
  :> Put '[JSON] CompleteIncomingRequestResp

data CompleteIncomingRequestReqBody
  = MarkCompletedReqBody
  | PayForReqBody
  { roundingEps :: Maybe (Positive Integer)
  , roundingStrategy :: Maybe RoundingStrategyReqBody
  }

instance FromJSON CompleteIncomingRequestReqBody where
  parseJSON = withObject "completeIncomingRequestBody" $ \obj ->
    obj .: "action" >>= \case
      "markCompleted" -> pure MarkCompletedReqBody
      "payFor" -> PayForReqBody
        <$> obj .:? "roundingEps"
        <*> obj .:? "roundingStrategy"
      unknownAction -> fail $ "Unknown action: " ++ unknownAction

instance ToJSON CompleteIncomingRequestReqBody where
  toJSON MarkCompletedReqBody = object
    [ "action" .= ("markCompleted" :: Text) ]
  toJSON (PayForReqBody eps strategy) = object
    [ "action" .= ("payFor" :: Text)
    , "roundingEps" .= eps
    , "roundingStrategy" .= strategy
    ]

data RoundingStrategyReqBody
  = RoundUpReqBody
  | RoundToNearestReqBody
  | RoundDownReqBody

instance FromJSON RoundingStrategyReqBody where
  parseJSON = withText "roundingStrategy" $ \case
    "up"         -> pure RoundUpReqBody
    "toNearest"  -> pure RoundToNearestReqBody
    "down"       -> pure RoundDownReqBody
    other        -> fail $ "Unknown rounding strategy: " ++ show other

instance ToJSON RoundingStrategyReqBody where
  toJSON RoundUpReqBody = String "up"
  toJSON RoundToNearestReqBody = String "toNearest"
  toJSON RoundDownReqBody = String "down"

data CompleteIncomingRequestResp
  = MarkedCompletedResp
  | PayedForResp BudgetResp

instance ToJSON CompleteIncomingRequestResp where
  toJSON (PayedForResp budget) = object
    [ "result" .= ("payedFor" :: Text)
    , "budget" .= budget
    ]
  toJSON MarkedCompletedResp = object
    [ "result" .= ("markedCompleted" :: Text) ]

instance FromJSON CompleteIncomingRequestResp where
  parseJSON = withObject "completeIncomingRequestResponse" $ \obj ->
    obj .: "result" >>= \case
      ("payedFor" :: Text) -> PayedForResp <$> obj .: "budget"
      "markedCompleted" -> return MarkedCompletedResp
      _ -> fail "invalid result value"
