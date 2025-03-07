{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module WebAPI.Users.Get
  ( Dependencies
  , getMe
  , UserResp(..), toResp
  ) where

import Servant (ServerT)
import Optics ((^?), (%), to, (<&>), (&))

import Core.Common.Operators ((^^.))
import Core.Users.Domain.UserType (UserType(Single))
import Core.Users.Domain.User (User)
import Core.Users.Domain.UserId (UserId(UserId))
import qualified Core.Users.GetSingle as Impl (getSingle, Dependencies)
import qualified WebAPI.Users.Budget.Get as Budget (toResp)
import Core.Users.CreateExistingSingle (createExistingSingle)
import Core.Users.Domain.Primitives (Username(Username))
import CheckCheck.Contracts.Users (GetMe, UserResp (..), AuthenticatedUser (..))

type Dependencies m = (Impl.Dependencies m)
getMe :: Dependencies m => AuthenticatedUser -> ServerT GetMe m
getMe AUser{ userId, username } = userId
   &  UserId
   &  Impl.getSingle
  <&> fmap toResp
  >>= maybe createMe return
  where
    createMe =
      toResp <$> createExistingSingle (UserId userId) (Username username)

toResp :: User 'Single -> UserResp
toResp user = UserResp
  { userId = user ^^. #userId
  , username = user ^^. #data % #username
  , budget = user ^? #data % #mBudget % to Budget.toResp
  }
