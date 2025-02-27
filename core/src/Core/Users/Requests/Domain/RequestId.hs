{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Core.Users.Requests.Domain.RequestId (RequestId(..), SomeRequestId(..), newRequestId) where

import Data.UUID (UUID)

import Core.Users.Requests.Domain.RequestStatus (RequestStatus (Pending))
import Core.Common.MonadClasses.MonadUUID (MonadUUID (newUUID))
import Optics (LabelOptic (labelOptic), A_Lens, lensVL, (<&>))

newtype RequestId (status :: RequestStatus) = RequestId { value :: UUID }

instance LabelOptic "value" A_Lens (RequestId t) (RequestId t) UUID UUID where
  labelOptic = lensVL $ \f (RequestId requestId) ->
    f requestId <&> RequestId

data SomeRequestId where
  SomeRequestId :: RequestId status -> SomeRequestId

instance LabelOptic "value" A_Lens SomeRequestId SomeRequestId UUID UUID where
  labelOptic = lensVL $ \f (SomeRequestId (RequestId requestId)) ->
    f requestId <&> SomeRequestId . RequestId

newRequestId :: MonadUUID m => m (RequestId 'Pending)
newRequestId = RequestId <$> newUUID
