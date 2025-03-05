{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module WebAPI.Users.Budget.Create (Dependencies, CreateBudget, createBudget) where

import Servant (ReqBody, JSON, (:>), Post, HasServer (ServerT), err404, errBody, ServerError)
import Control.Monad.Error.Class (MonadError (throwError))

import qualified Core.Users.Budget.Create as Impl (Data(..), create, Dependencies, Error(UserDoesNotExist))
import WebAPI.Auth (AuthenticatedUser (..))
import Core.Users.Domain.UserId (SomeUserId(SomeUserId))
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Core.Common.Domain.RubKopecks (RubKopecks(RubKopecks))
import WebAPI.Users.Budget.Get (BudgetResp, toResp)
import Data.UUID (toLazyASCIIBytes)

type CreateBudget =
  ReqBody '[JSON] CreateBudgetReqBody :> Post '[JSON] BudgetResp

data CreateBudgetReqBody = CreateBudgetReqBody
  { initialBudget :: Maybe Integer
  , lowerBound :: Maybe Integer
  } deriving (Generic, FromJSON)

type Dependencies m = (Impl.Dependencies m, MonadError ServerError m)
createBudget :: Dependencies m => AuthenticatedUser -> ServerT CreateBudget m
createBudget AUser{ userId } CreateBudgetReqBody{..} =
  Impl.create (SomeUserId userId) Impl.Data
    { Impl.mInitialAmount = RubKopecks <$> initialBudget
    , Impl.mLowerBound = RubKopecks <$> lowerBound
    } >>= either mapError (return . toResp)
  where
    mapError (Impl.UserDoesNotExist(SomeUserId uuid)) =
      throwError err404{ errBody = toLazyASCIIBytes uuid }
