{-# LANGUAGE DataKinds #-}
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

module WebAPI.Groups.OutgoingRequests.Send
  ( Dependencies
  , SendRequestReqBody
  , SendRequest, sendRequest
  ) where

import Servant (ServerT, throwError, err404, ServerError, errBody, err400)
import Data.UUID (toLazyASCIIBytes, UUID)
import qualified Data.Text as T
import Control.Monad.Except (MonadError)
import Optics ((^.), (<&>), (&))

import Data.List.NonEmpty (toList)
import Data.String (fromString)

import Core.Common.Domain.RubKopecks (positiveRubKopecks)
import Core.Users.Domain.UserId (SomeUserId(SomeUserId))
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
import CheckCheck.Contracts.Users.OutgoingRequests (SendRequestReqBody(..), SendRequest, SendListRequestItemReqBody (..), IndexSelectionReqBody (..), RequestResp (..))
import CheckCheck.Contracts.Users (AuthenticatedUser (..))

type Dependencies m =
  ( ListImpl.Dependencies m
  , ReceiptItemsImpl.Dependencies m
  , MonadError ServerError m
  )
sendRequest :: Dependencies m => AuthenticatedUser -> UUID -> ServerT SendRequest m
sendRequest AUser{} groupId SendListRequestReqBody{ recipientId, list } = do
  -- TODO check somewhere that the user is a part of the group
  let data' = ListImpl.Data
        { senderId = groupId & SomeUserId
        , recipientId = recipientId & SomeUserId
        , list = list <&> \SendListRequestItemReqBody{..} -> ListImpl.Item
          { price = positiveRubKopecks price, ..}
        }
  ListImpl.sendRequest data' >>= \case
    Right req -> return [toResp req]
    Left (ListImpl.UserDoesNotExist (SomeUserId uuid)) ->
      throwError err404{ errBody = toLazyASCIIBytes uuid }

sendRequest AUser{} groupId SendReceiptItemsRequestReqBody{ receiptQr, indexSelections } = do
  let data' = ReceiptItemsImpl.Data
        { senderId = groupId & SomeUserId
        , indexSelections = indexSelections <&> \IndexSelectionReqBody{..} ->
            ReceiptItemsImpl.IndexSelection{ recipientId = recipientId & SomeUserId, .. }
        , ..
        }
  ReceiptItemsImpl.sendRequest data' >>= \case
    Right reqs -> return $ toResp <$> toList reqs
    Left (ReceiptItemsImpl.UserDoesNotExist (SomeUserId uuid)) ->
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
