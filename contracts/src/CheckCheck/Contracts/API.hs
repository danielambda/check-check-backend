{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module CheckCheck.Contracts.API (API) where

import Servant.API ((:<|>)(..), (:>))

import CheckCheck.Contracts.Receipts (ReceiptsAPI)
import CheckCheck.Contracts.Users (UsersAPI)
import CheckCheck.Contracts.Groups (GroupsAPI)

type API
  =    "receipts" :> ReceiptsAPI
  :<|> "groups" :> GroupsAPI
  :<|> UsersAPI
