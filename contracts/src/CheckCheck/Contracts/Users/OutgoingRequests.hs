{-# OPTIONS_GHC -Wno-partial-fields #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module CheckCheck.Contracts.Users.OutgoingRequests
  ( OutgoingRequestsAPI
  , SendRequest
  , SendRequestReqBody(..)
  , IndexSelectionReqBody(..)
  , SendListRequestItemReqBody(..)
  , RequestResp(..)
  ) where

import Servant.API (ReqBody, JSON, (:>), Post)
import Data.UUID (UUID)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON (parseJSON), Options (sumEncoding), ToJSON (toJSON), genericParseJSON, defaultOptions, SumEncoding (UntaggedValue), genericToJSON)
import SmartPrimitives.Positive (Positive)

type OutgoingRequestsAPI = SendRequest

type SendRequest =
  ReqBody '[JSON] SendRequestReqBody :> Post '[JSON] [RequestResp]

data SendRequestReqBody
  = SendListRequestReqBody
  { recipientId :: UUID
  , list :: NonEmpty SendListRequestItemReqBody
  }
  | SendReceiptItemsRequestReqBody
  { receiptQr :: Text
  , indexSelections :: NonEmpty IndexSelectionReqBody
  } deriving (Generic)
instance FromJSON SendRequestReqBody where
  parseJSON = genericParseJSON defaultOptions{ sumEncoding = UntaggedValue }
instance ToJSON SendRequestReqBody where
  toJSON = genericToJSON defaultOptions{ sumEncoding = UntaggedValue }

data IndexSelectionReqBody = IndexSelectionReqBody
  { recipientId :: UUID
  , indices :: NonEmpty Int
  } deriving (Generic, FromJSON, ToJSON)

data SendListRequestItemReqBody = SendListRequestItemReqBody
  { identity :: Text
  , quantity :: Positive Double
  , price :: Positive Integer
  } deriving (Generic, FromJSON, ToJSON)

data RequestResp = RequestResp
  { requestId :: UUID
  , senderId :: UUID
  , recipientId :: UUID
  , createdAt :: UTCTime
  , isPending :: Bool
  } deriving (Generic, ToJSON, FromJSON)
