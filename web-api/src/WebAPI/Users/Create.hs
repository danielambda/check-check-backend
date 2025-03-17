{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module WebAPI.Users.Create (createMe, Dependencies) where

import CheckCheck.Contracts.Users (AuthenticatedUser (..), CreateMe)
import Servant (ServerT, WithStatus (WithStatus), respond)
import Core.Users.Domain.UserId (UserId(UserId))
import qualified Core.Users.CreateSingle as Impl (Data(..), createSingle, Dependencies)
import WebAPI.Users.Get (toResp)

type Dependencies m = (Impl.Dependencies m)
createMe :: Dependencies m => AuthenticatedUser -> ServerT CreateMe m
createMe AUser{ userId, username } = do
  let data' = Impl.Data (UserId userId) username
  Impl.createSingle data' >>= \case
    Right a -> respond $ WithStatus @201 $ toResp a
    Left err -> respond $ WithStatus @400 err
