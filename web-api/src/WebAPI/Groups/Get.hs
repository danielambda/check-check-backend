{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module WebAPI.Groups.Get
  ( Dependencies
  , getGroup
  , toResp
  ) where

import Servant (ServerT, err404, ServerError, err403)
import Optics ((%), to, (^?), summing)

import Control.Monad.Error.Class (throwError, MonadError)

import Core.Common.Operators ((^^.), (^^..))
import Core.Users.Domain.UserId (UserId(UserId))
import Core.Users.Domain.User (User)
import Core.Users.Domain.UserType (UserType(Group))
import qualified Core.Users.GetGroup as Impl (getGroup, Dependencies)
import qualified WebAPI.Users.Budget.Get as Budget (toResp)
import CheckCheck.Contracts.Groups (GetGroup, GroupResp (..))
import CheckCheck.Contracts.Users (AuthenticatedUser(..))

type Dependencies m = (Impl.Dependencies m, MonadError ServerError m)
getGroup :: Dependencies m => AuthenticatedUser -> ServerT GetGroup m
getGroup AUser{ userId } groupId = Impl.getGroup (UserId groupId) >>= \case
  Nothing -> throwError err404
  Just group -> if userId `elem` group ^^.. (#ownerId `summing` #otherUserIds)
    then return $ toResp group
    else throwError err403

toResp :: User 'Group -> GroupResp
toResp group = GroupResp
  { groupId = group ^^. #userId
  , name = group ^^. #data % #username
  , ownerId = group ^^. #ownerId
  , otherUserIds = group ^^.. #otherUserIds
  , budget = group ^? #data % #mBudget % to Budget.toResp
  }

