{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}

module Core.Users.Domain.UserId (UserId(..), SomeUserId(..), newUserId) where

import Optics (LabelOptic (labelOptic), A_Lens, lensVL, (<&>))
import Data.UUID (UUID)

import Core.Users.Domain.UserType (UserType)
import Core.Common.MonadClasses.MonadUUID (MonadUUID (newUUID))

newtype UserId (t :: UserType) = UserId { value :: UUID }

instance LabelOptic "value" A_Lens (UserId t) (UserId t) UUID UUID where
  labelOptic = lensVL $ \f (UserId userId) ->
    f userId <&> UserId

data SomeUserId where
  SomeUserId :: UserId t -> SomeUserId

instance LabelOptic "value" A_Lens SomeUserId SomeUserId UUID UUID where
  labelOptic = lensVL $ \f (SomeUserId (UserId userId)) ->
    f userId <&> SomeUserId . UserId

newUserId :: MonadUUID m => m (UserId t)
newUserId = UserId <$> newUUID
