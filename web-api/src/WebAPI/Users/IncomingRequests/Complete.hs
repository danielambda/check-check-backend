{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

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
import Data.Aeson (FromJSON (parseJSON), withText, withObject, (.:), (.:?), ToJSON (toJSON), object, KeyValue ((.=)))
import Data.Text (Text)
import Optics ((&), (^.))
import Control.Monad.Error.Class (MonadError)

import SmartPrimitives.Positive (Positive)
import Core.Common.Operators ((^^.), (^^?))
import Core.Common.Domain.RubKopecks (positiveRubKopecks)
import Core.Users.Budget.Domain.Budget (RoundingData'(RoundingData), RoundingStrategy (..), BudgetLowerBoundStatus (..))
import Core.Users.Domain.UserId (SomeUserId(SomeUserId))
import Core.Users.Requests.Domain.RequestId (RequestId(RequestId))
import qualified Core.Users.Requests.MarkCompleted as MarkImpl
  (Dependencies, Data(..), Error (..), markRequestCompleted)
import qualified Core.Users.Requests.PayFor as PayImpl
  (Dependencies, Data(..), Error (..), payForRequest)
import WebAPI.Users.Budget.Get (BudgetResp(..))
import WebAPI.Auth (AuthenticatedUser (AUser, userId))

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

data CompleteIncomingRequestResp
  = MarkedCompletedResp
  | PayedForResp BudgetResp
instance ToJSON CompleteIncomingRequestResp where
  toJSON MarkedCompletedResp = object
    [ "result" .= ("markedCompleted" :: Text) ]
  toJSON (PayedForResp budget) = object
    [ "result" .= ("payedFor" :: Text)
    , "budget" .= budget
    ]

type Dependencies m = (MarkImpl.Dependencies m, PayImpl.Dependencies m, MonadError ServerError m)
completeIncomingRequest :: Dependencies m => AuthenticatedUser -> ServerT CompleteIncomingRequest m
completeIncomingRequest AUser{ userId } requestId = \case
  MarkCompletedReqBody -> do
    let data' = MarkImpl.Data
          { MarkImpl.recipientId = userId & SomeUserId
          , MarkImpl.requestId = requestId & RequestId
          }
    MarkImpl.markRequestCompleted data' >>= \case
      Right () -> return MarkedCompletedResp
      Left (MarkImpl.RequestDoesNotExist(RequestId uuid)) ->
        throwError err404{ errBody = toLazyASCIIBytes uuid }
      Left (MarkImpl.UserDoesNotExist(SomeUserId uuid)) ->
        throwError err404{ errBody = toLazyASCIIBytes uuid }
      Left (MarkImpl.RequestIsNotPending(RequestId uuid)) ->
        throwError err400{ errBody = "Request "<>toLazyASCIIBytes uuid<>" is not pending" }

  PayForReqBody{ roundingEps, roundingStrategy } -> do
    let mRoundingData = RoundingData
          <$> fmap positiveRubKopecks roundingEps
          <*> fmap mapRoundingStrategy roundingStrategy
    let data' = PayImpl.Data
          { PayImpl.recipientId = userId & SomeUserId
          , PayImpl.requestId = requestId & RequestId
          , PayImpl.mRoundingData = mRoundingData
          }
    PayImpl.payForRequest data' >>= \case
      Right budget -> return $ PayedForResp $ budgetResp budget
        where
          budgetResp b = BudgetResp
            { amount = b ^^. #amount
            , lowerBound = b ^^? #mLowerBound
            , isLowerBoundExceeded = b ^. #lowerBoundStatus == BudgetLowerBoundExceeded
            }
      Left (PayImpl.UserDoesNotExist(SomeUserId uuid)) ->
        throwError err404{ errBody = toLazyASCIIBytes uuid }
      Left (PayImpl.UserDoesNotHaveBudget(SomeUserId uuid)) ->
        throwError err400{ errBody = "User "<>toLazyASCIIBytes uuid<>" does not have a budget" }
      Left (PayImpl.RequestDoesNotExist(RequestId uuid)) ->
        throwError err404{ errBody = toLazyASCIIBytes uuid }
      Left (PayImpl.RequestIsNotPending(RequestId uuid)) ->
        throwError err400{ errBody = "Request "<>toLazyASCIIBytes uuid<>" is not pending" }

mapRoundingStrategy :: RoundingStrategyReqBody -> RoundingStrategy
mapRoundingStrategy RoundUpReqBody = RoundUp
mapRoundingStrategy RoundToNearestReqBody = RoundToNearest
mapRoundingStrategy RoundDownReqBody = RoundDown

