{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Core.Users.Requests.SendList
  ( Data(..), Item(..)
  , Dependencies
  , sendRequest
  , Error(..)
  ) where

import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)

import Data.Text (Text)

import SmartPrimitives.Positive (Positive)
import Core.Common.Operators ((*>>))
import Core.Common.MonadClasses.MonadUUID (MonadUUID)
import Core.Common.MonadClasses.MonadUTCTime (MonadUTCTime)
import Core.Common.Domain.RubKopecks (RubKopecks)
import Core.Users.Domain.UserId (SomeUserId)
import Core.Users.MonadClasses.Repository (UsersRepository (userExistsInRepo))
import Core.Users.Requests.Domain.Request (newRequest, Request, RequestItem (..), RequestItemIdentity (..))
import Core.Users.Requests.Domain.RequestStatus (RequestStatus(Pending))
import Core.Users.Requests.MonadClasses.Repository (RequestsRepository (addRequestToRepo))

data Data = Data
  { senderId :: SomeUserId
  , recipientId :: SomeUserId
  , list :: NonEmpty Item
  }

data Item = Item
  { identity :: Text
  , quantity :: Positive Double
  , price :: Positive RubKopecks
  }

newtype Error = UserDoesNotExist SomeUserId

type Dependencies m = (MonadUUID m, MonadUTCTime m, RequestsRepository m, UsersRepository m)
sendRequest :: Dependencies m => Data -> m (Either Error (Request 'Pending))
sendRequest Data{ senderId, recipientId, list } = do
  -- TODO I do not like the validation flow
  senderExists <- userExistsInRepo senderId
  if senderExists then do
    recipientExists <- userExistsInRepo recipientId
    if recipientExists then do
      let items = list <&> \Item{..} -> RequestItem
            { identity = TextIdentity identity , .. }
      newRequest senderId recipientId items >>=
        (addRequestToRepo *>> return . Right)
    else
      return $ Left $ UserDoesNotExist recipientId
  else
    return $ Left $ UserDoesNotExist senderId
