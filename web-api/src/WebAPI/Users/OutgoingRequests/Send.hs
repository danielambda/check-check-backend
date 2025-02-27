{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wno-partial-fields #-}

module WebAPI.Users.OutgoingRequests.Send
  ( Dependencies
  , SendRequestReqBody
  , SendRequest, sendRequest
  ) where

import Servant (ServerT, (:>), JSON, Post, ReqBody, throwError, err404, ServerError, errBody, err400)
import Data.Aeson
  ( FromJSON (parseJSON), ToJSON
  , genericParseJSON, defaultOptions
  , Options (sumEncoding), SumEncoding (UntaggedValue)
  )
import Data.UUID (UUID, toLazyASCIIBytes)
import Data.Text as T
import Control.Monad.Except (MonadError)
import Optics ((^.), (<&>), (&))

import GHC.Generics (Generic)
import Data.Time (UTCTime)
import Data.List.NonEmpty (NonEmpty, toList)

import SmartPrimitives.Positive (Positive)
import SmartPrimitives.NonNegative (NonNegative)
import Core.Common.Domain.RubKopecks (positiveRubKopecks)
import Core.Users.Domain.UserId (SomeUserId(SomeUserId), UserId (UserId))
import Core.Users.Requests.Domain.Request (Request(..))
import Core.Users.Requests.Domain.RequestStatus (RequestStatus(Pending))
import qualified Core.Users.Requests.SendList as ListImpl
  ( Dependencies, Data(..), Item(..)
  , sendRequest , Error(..)
  )
import qualified Core.Users.Requests.SendReceiptItems as ReceiptItemsImpl
  ( Dependencies, Data(..), IndexSelection(..)
  , sendRequest , Error(..)
  )
import Data.String (fromString)

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

data IndexSelectionReqBody = IndexSelectionReqBody
  { recipientId :: UUID
  , indices :: NonEmpty (NonNegative Int)
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
  } deriving (Generic, ToJSON)

type Dependencies m =
  ( ListImpl.Dependencies m
  , ReceiptItemsImpl.Dependencies m
  , MonadError ServerError m
  )
sendRequest :: Dependencies m => UUID -> ServerT SendRequest m
sendRequest senderId SendListRequestReqBody{ recipientId, list } = do
  let data' = ListImpl.Data
        { senderId = senderId & SomeUserId . UserId
        , recipientId = recipientId & SomeUserId . UserId
        , list = list <&> \SendListRequestItemReqBody{..} -> ListImpl.Item
          { price = positiveRubKopecks price, ..}
        }
  ListImpl.sendRequest data' >>= \case
    Right req -> return [toResp req]
    Left (ListImpl.UserDoesNotExist (SomeUserId (UserId uuid))) ->
      throwError err404{ errBody = toLazyASCIIBytes uuid }

sendRequest senderId SendReceiptItemsRequestReqBody{ receiptQr, indexSelections } = do
  let data' = ReceiptItemsImpl.Data
        { senderId = senderId & SomeUserId . UserId
        , indexSelections = indexSelections <&> \IndexSelectionReqBody{..} ->
            ReceiptItemsImpl.IndexSelection{recipientId = recipientId & SomeUserId . UserId, ..}
        , ..
        }
  ReceiptItemsImpl.sendRequest data' >>= \case
    Right reqs -> return $ toResp <$> toList reqs
    Left (ReceiptItemsImpl.UserDoesNotExist (SomeUserId (UserId uuid))) ->
      throwError err404{ errBody = toLazyASCIIBytes uuid }
    Left (ReceiptItemsImpl.ReceiptDoesNotExist qr) ->
      throwError err404{ errBody = fromString $ T.unpack qr }
    Left (ReceiptItemsImpl.ReceiptIndexOutOfRange _ _) ->
      throwError err400{ errBody = "Receipt Index was out of range" }
    Left _ -> error "abobus " -- TODO I have no idea what is this

toResp :: Request 'Pending -> RequestResp
toResp Request{..} = RequestResp
  { requestId = requestId ^. #value
  , senderId = senderId ^. #value
  , recipientId = recipientId ^. #value
  , isPending = True
  , ..
  }
