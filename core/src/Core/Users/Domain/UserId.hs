{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}

module Core.Users.Domain.UserId (UserId(..), SomeUserId(..), someUserId, newUserId) where

import Optics (LabelOptic (labelOptic), A_Lens, lensVL, (<&>), A_Getter, to)
import Data.UUID (UUID)

import Core.Users.Domain.UserType (UserType)
import Core.Common.MonadClasses.MonadUUID (MonadUUID (newUUID))
import Data.Data (Typeable)

data UserId (t :: UserType) where
  UserId :: Typeable t => UUID -> UserId t

instance LabelOptic "value" A_Lens (UserId t) (UserId t) UUID UUID where
  labelOptic = lensVL $ \f (UserId userId) ->
    f userId <&> UserId

newtype SomeUserId = SomeUserId UUID

someUserId :: UserId t -> SomeUserId
someUserId (UserId userId) = SomeUserId userId

instance LabelOptic "value" A_Getter SomeUserId SomeUserId UUID UUID where
  labelOptic = to $ \(SomeUserId userId) -> userId

newUserId :: (MonadUUID m, Typeable t) => m (UserId t)
newUserId = UserId <$> newUUID
