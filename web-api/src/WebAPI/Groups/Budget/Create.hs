{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module WebAPI.Groups.Budget.Create (Dependencies, createBudget) where

import Servant (HasServer (ServerT), err404, err400, errBody, ServerError)
import Control.Monad.Error.Class (MonadError (throwError))

import Core.Users.Domain.UserId (SomeUserId(SomeUserId))
import Core.Common.Domain.RubKopecks (RubKopecks(RubKopecks))
import WebAPI.Users.Budget.Get (toResp)
import Data.UUID (toLazyASCIIBytes, UUID)
import qualified Core.Users.Budget.Create as Impl (Data(..), create, Dependencies, Error(..))
import CheckCheck.Contracts.Users.Budget (CreateBudgetReqBody(..), CreateBudget)
import CheckCheck.Contracts.Users (AuthenticatedUser(..))

type Dependencies m = (Impl.Dependencies m, MonadError ServerError m)
createBudget :: Dependencies m => AuthenticatedUser -> UUID -> ServerT CreateBudget m
createBudget AUser{} groupId CreateBudgetReqBody{..} =
  Impl.create (SomeUserId groupId) Impl.Data
    { Impl.mInitialAmount = RubKopecks <$> initialBudget
    , Impl.mLowerBound = RubKopecks <$> lowerBound
    } >>= either mapError (return . toResp)
  where
    mapError (Impl.UserDoesNotExist(SomeUserId uuid)) =
      throwError err404{ errBody = toLazyASCIIBytes uuid }
    mapError (Impl.UserAlreadyHasBudget(SomeUserId uuid)) =
      throwError err400{ errBody = toLazyASCIIBytes uuid }

