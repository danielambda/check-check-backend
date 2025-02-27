{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WebAPI.Users.IncomingRequests.Complete
  ( Dependencies
  , CompleteIncomingRequest
  , completeIncomingRequest
  ) where

import Servant (Capture, (:>), Put, JSON, HasServer (ServerT), ReqBody, throwError, err404, errBody, err400, ServerError)
import Data.UUID (UUID, toLazyASCIIBytes)
import Data.Aeson (FromJSON (parseJSON), withText, withObject, (.:), (.:?))
import Optics ((&))
import Control.Monad.Error.Class (MonadError)

import SmartPrimitives.Positive (Positive)
import Core.Common.Domain.RubKopecks (positiveRubKopecks)
import Core.Users.Budget.Domain.Budget (RoundingData(RoundingData), RoundingStrategy (..))
import Core.Users.Domain.UserId (SomeUserId(SomeUserId), UserId (UserId))
import Core.Users.Requests.Domain.RequestId (RequestId(RequestId))
import qualified Core.Users.Requests.MarkCompleted as MarkImpl
  (Dependencies, Data(..), Error (..), markRequestCompleted)
import qualified Core.Users.Requests.PayFor as PayImpl
  (Dependencies, Data(..), Error (..), payForRequest)

type CompleteIncomingRequest =
  Capture "requestId" UUID :> ReqBody '[JSON] CompleteIncomingRequestReqBody :> Put '[JSON] Int

data CompleteIncomingRequestReqBody
  = MarkIncomingRequestCompletedReqBody
  | PayForIncomingRequestReqBody
  { roundingEps :: Maybe (Positive Integer)
  , roudingStrategy :: Maybe RoundingStrategyReqBody
  }
instance FromJSON CompleteIncomingRequestReqBody where
  parseJSON = withObject "completeIncomingRequestBody" $ \obj ->
    obj .: "action" >>= \case
      "markCompleted" -> pure MarkIncomingRequestCompletedReqBody
      "payFor" -> PayForIncomingRequestReqBody
        <$> obj .:? "roundingEps"
        <*> obj .:? "roundingStrategy"
      unknownAction -> fail $ "Unknown action: " ++ unknownAction

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

type Dependencies m = (MarkImpl.Dependencies m, PayImpl.Dependencies m, MonadError ServerError m)
completeIncomingRequest :: Dependencies m => UUID -> ServerT CompleteIncomingRequest m
completeIncomingRequest recipientId requestId
  MarkIncomingRequestCompletedReqBody = do
  let data' = MarkImpl.Data
        { MarkImpl.recipientId = recipientId & SomeUserId . UserId
        , MarkImpl.requestId = requestId & RequestId
        }
  MarkImpl.markRequestCompleted data' >>= \case
    Right () -> return 0
    Left (MarkImpl.RequestDoesNotExist(RequestId uuid)) ->
      throwError err404{ errBody = toLazyASCIIBytes uuid }
    Left (MarkImpl.UserDoesNotExist(SomeUserId(UserId uuid))) ->
      throwError err404{ errBody = toLazyASCIIBytes uuid }
    Left (MarkImpl.RequestIsNotPending(RequestId uuid)) ->
      throwError err400{ errBody = "Request "<>toLazyASCIIBytes uuid<>" is not pending" }

completeIncomingRequest recipientId requestId
  PayForIncomingRequestReqBody{ roundingEps, roudingStrategy } = do
  let mRoundingData = RoundingData
        <$> fmap positiveRubKopecks roundingEps
        <*> fmap mapRoundingStrategy roudingStrategy
  let data' = PayImpl.Data
        { PayImpl.recipientId = recipientId & SomeUserId . UserId
        , PayImpl.requestId = requestId & RequestId
        , PayImpl.mRoundingData = mRoundingData
        }
  PayImpl.payForRequest data' >>= \case
    Right () -> return 0
    Left (PayImpl.RequestDoesNotExist(RequestId uuid)) ->
      throwError err404{ errBody = toLazyASCIIBytes uuid }
    Left (PayImpl.UserDoesNotExist(SomeUserId(UserId uuid))) ->
      throwError err404{ errBody = toLazyASCIIBytes uuid }
    Left (PayImpl.RequestIsNotPending(RequestId uuid)) ->
      throwError err400{ errBody = "Request "<>toLazyASCIIBytes uuid<>" is not pending" }

mapRoundingStrategy :: RoundingStrategyReqBody -> RoundingStrategy
mapRoundingStrategy RoundUpReqBody = RoundUp
mapRoundingStrategy RoundToNearestReqBody = RoundToNearest
mapRoundingStrategy RoundDownReqBody = RoundDown

