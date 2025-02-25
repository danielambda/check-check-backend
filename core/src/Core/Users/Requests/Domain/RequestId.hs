{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Core.Users.Requests.Domain.RequestId (RequestId(..), newRequestId) where

import Data.UUID (UUID)

import Core.Users.Requests.Domain.RequestStatus (RequestStatus (Pending))
import Core.Common.MonadClasses.MonadUUID (MonadUUID (newUUID))
import Optics (LabelOptic (labelOptic), A_Lens, lensVL, (<&>))

newtype RequestId (status :: RequestStatus) = RequestId { value :: UUID }

instance LabelOptic "value" A_Lens (RequestId t) (RequestId t) UUID UUID where
  labelOptic = lensVL $ \f (RequestId userId) ->
    f userId <&> RequestId


newRequestId :: MonadUUID m => m (RequestId 'Pending)
newRequestId = RequestId <$> newUUID
