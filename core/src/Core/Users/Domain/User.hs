{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Core.Users.Domain.User
  ( User(..), SomeUser(..)
  , UserData(..)
  , newUserSingle, newUserGroup
  ) where

import Optics
  ( LabelOptic, labelOptic
  , An_AffineTraversal, atraversalVL
  , A_Lens, lensVL
  , (<&>), makeFieldLabelsFor, A_Traversal, traversalVL
  )

import Core.Common.MonadClasses.MonadUUID (MonadUUID)
import Core.Users.Budget.Domain.Budget (Budget)
import Core.Users.Domain.Primitives (Username)
import Core.Users.Domain.UserType (UserType(..))
import Core.Users.Domain.UserId (UserId, newUserId)

data UserData = UserData
  { username :: Username
  , mBudget :: Maybe Budget
  }

data User (t :: UserType) where
  UserSingle ::
    { userSingleId :: UserId 'Single
    , userSingleData :: UserData
    } -> User 'Single
  UserGroup ::
    { userGroupId :: UserId Group
    , ownerId :: UserId 'Single
    , otherUserIds :: [UserId 'Single]
    , userGroupData :: UserData
    } -> User 'Group

newUserSingle :: MonadUUID m => UserData -> m (User 'Single)
newUserSingle userData = do
  userId <- newUserId
  return $ UserSingle userId userData

newUserGroup :: MonadUUID m => UserId 'Single -> [UserId 'Single] -> UserData -> m (User 'Group)
newUserGroup ownerId otherUserIds userData = do
  userId <- newUserId
  return $ UserGroup userId ownerId otherUserIds userData

data SomeUser where SomeUser :: User t -> SomeUser

-- Optics --

makeFieldLabelsFor [("username", "username")] ''UserData

instance (k ~ An_AffineTraversal, a ~ Budget, a ~ b) => LabelOptic "mBudget" k UserData UserData a b where
  labelOptic = atraversalVL $ \point f UserData{mBudget, ..} -> case mBudget of
    Just budget -> f budget <&> \budget' -> UserData{mBudget = Just budget', ..}
    Nothing -> point UserData{..}

instance (a ~ UserId t, a ~ b) => LabelOptic "userId" A_Lens (User t) (User t) a b where
  labelOptic = lensVL $ \f user -> case user of
    UserSingle{userSingleId, ..} -> f userSingleId <&> \userId' -> UserSingle{userSingleId = userId', ..}
    UserGroup{userGroupId, ..}   -> f userGroupId <&>  \userId' -> UserGroup{userGroupId = userId', ..}

instance (k ~ A_Lens, a ~ UserData, a ~ b) => LabelOptic "data" k (User t) (User t) a b where
  labelOptic = lensVL $ \f user -> case user of
    UserSingle{userSingleData, ..} -> f userSingleData <&> \data' -> UserSingle{userSingleData = data', ..}
    UserGroup{userGroupData, ..}   -> f userGroupData <&>  \data' -> UserGroup{userGroupData = data', ..}

instance (k ~ An_AffineTraversal, a ~ UserId 'Single, a ~ b) => LabelOptic "mOwnerId" k (User t) (User t) a b where
  labelOptic = atraversalVL $ \point f user -> case user of
    UserSingle{..} -> point UserSingle{..}
    UserGroup{ownerId, ..} -> f ownerId <&> \ownerId' -> UserGroup{ownerId = ownerId', ..}

instance (k ~ A_Lens, a ~ UserId 'Single, a ~ b) => LabelOptic "ownerId" k (User 'Group) (User 'Group) a b where
  labelOptic = lensVL $ \f UserGroup{ownerId, ..} ->
    f ownerId <&> \ownerId' -> UserGroup{ownerId = ownerId', ..}

instance (k ~ A_Traversal, a ~ UserId 'Single, a ~ b) => LabelOptic "otherUserIds" k (User t) (User t) a b where
  labelOptic = traversalVL $ \f user -> case user of
    UserSingle{..} -> pure UserSingle{..}
    UserGroup{otherUserIds, ..} -> traverse f otherUserIds <&> \o -> UserGroup{otherUserIds = o, ..}
