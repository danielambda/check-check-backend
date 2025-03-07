{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WebAPI.Groups.IncomingRequests.Complete
  ( Dependencies
  , completeIncomingRequest
  ) where

import Servant (HasServer (ServerT), throwError, err404, errBody, err400, ServerError)
import Data.UUID (toLazyASCIIBytes, UUID)
import Optics ((&), (^.))
import Control.Monad.Error.Class (MonadError)

import Core.Common.Operators ((^^.), (^^?))
import Core.Common.Domain.RubKopecks (positiveRubKopecks)
import Core.Users.Budget.Domain.Budget (RoundingData'(RoundingData), RoundingStrategy (..), BudgetLowerBoundStatus (..))
import Core.Users.Domain.UserId (SomeUserId(SomeUserId))
import Core.Users.Requests.Domain.RequestId (RequestId(RequestId))
import qualified Core.Users.Requests.MarkCompleted as MarkImpl
  (Dependencies, Data(..), Error (..), markRequestCompleted)
import qualified Core.Users.Requests.PayFor as PayImpl
  (Dependencies, Data(..), Error (..), payForRequest)
import CheckCheck.Contracts.Users (AuthenticatedUser(..))
import CheckCheck.Contracts.Users.IncomingRequests (CompleteIncomingRequestReqBody(..), CompleteIncomingRequest, RoundingStrategyReqBody (..), CompleteIncomingRequestResp (..))
import CheckCheck.Contracts.Users.Budget (BudgetResp(..))

type Dependencies m = (MarkImpl.Dependencies m, PayImpl.Dependencies m, MonadError ServerError m)
completeIncomingRequest :: Dependencies m
                        => AuthenticatedUser -> UUID -> ServerT CompleteIncomingRequest m
completeIncomingRequest AUser{} groupId requestId = \case
  MarkCompletedReqBody -> do
    let data' = MarkImpl.Data
          { MarkImpl.recipientId = groupId & SomeUserId
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
          { PayImpl.recipientId = groupId & SomeUserId
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

