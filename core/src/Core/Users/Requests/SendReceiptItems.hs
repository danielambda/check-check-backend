{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module Core.Users.Requests.SendReceiptItems
  ( Data(..), IndexSelection(..)
  , Dependencies, sendRequest
  , Error(..)
  ) where

import Data.Text (Text)
import Optics ((^.))

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty ((!!))
import Control.Monad (forM)

import SmartPrimitives.NonNegative (NonNegative, unNonNegative, nonNegativeLength)
import Core.Common.Operators ((*>>))
import Core.Common.MonadClasses.MonadUUID (MonadUUID)
import Core.Common.MonadClasses.MonadUTCTime (MonadUTCTime)
import Core.Users.Domain.UserId (SomeUserId)
import Core.Users.Requests.Domain.Request (newRequest, Request, RequestItem (..), RequestItemIdentity (ReceiptItemNameIdentity))
import Core.Users.Requests.Domain.RequestStatus (RequestStatus(Pending))
import Core.Users.Requests.MonadClasses.Repository (RequestsRepository (addRequestToRepo))
import Core.Users.MonadClasses.Repository (UsersRepository (userExistsInRepo))
import Core.Receipts.Domain.Receipt (Receipt)
import Core.Receipts.Domain.ReceiptItem (ReceiptItem(..))
import qualified Core.Receipts.Get as Receipts (get, Dependencies)

data Data = Data
  { senderId :: SomeUserId
  , receiptQr :: Text
  , indexSelections :: NonEmpty IndexSelection
  }

data IndexSelection = IndexSelection
  { recipientId :: SomeUserId
  , indices :: NonEmpty (NonNegative Int)
  }

data Error
  = UserDoesNotExist SomeUserId
  | ReceiptDoesNotExist Text
  | ReceiptIndexOutOfRange (NonNegative Int) (NonNegative Int)
  | ReceiptIndexDuplicate (NonNegative Int)

type Dependencies m =
  ( MonadUUID m, MonadUTCTime m
  , RequestsRepository m
  , UsersRepository m
  , Receipts.Dependencies m
  )
sendRequest :: Dependencies m => Data -> m (Either Error (NonEmpty (Request 'Pending)))
sendRequest Data{ senderId, receiptQr, indexSelections } = do
-- TODO handle ReceiptIndexDuplicate
  senderExists <- userExistsInRepo senderId
  if not senderExists then
    return $ Left $ UserDoesNotExist senderId
  else Receipts.get receiptQr >>= \case
    Nothing ->
      return $ Left $ ReceiptDoesNotExist receiptQr
    Just receipt -> do
      let items = receipt ^. #items
      let receiptItemsCount = nonNegativeLength items
      fmap sequence $ forM indexSelections $
        createAndStoreRequest senderId receipt receiptItemsCount

createAndStoreRequest
  :: Dependencies m
  => SomeUserId
  -> Receipt
  -> NonNegative Int
  -> IndexSelection
  -> m (Either Error (Request 'Pending))
createAndStoreRequest senderId receipt receiptItemsCount
  IndexSelection{ recipientId, indices } = do
  let maxIndex = maximum indices
  if receiptItemsCount <= maxIndex then
    return $ Left $ ReceiptIndexOutOfRange maxIndex receiptItemsCount
  else do
    let indexedItems = receipt ^. #items
    let selectedItems = snd . (indexedItems NonEmpty.!!) . unNonNegative <$> indices
    let requestItems = toRequest <$> selectedItems
    newRequest senderId recipientId requestItems >>=
      (addRequestToRepo *>> return . Right)
  where
    toRequest :: ReceiptItem -> RequestItem
    toRequest ReceiptItem{..} = RequestItem
      { identity = ReceiptItemNameIdentity name , ..}

