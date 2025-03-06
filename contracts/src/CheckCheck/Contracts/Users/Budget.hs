{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}

module CheckCheck.Contracts.Users.Budget
  ( BudgetAPI
  , ApplyBudgetDeltaToUser
  , CreateBudget
  , CreateBudgetReqBody(..)
  , BudgetResp(..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Servant.API (ReqBody, JSON, (:>), Patch, (:<|>), Post)

type BudgetAPI
  =    ApplyBudgetDeltaToUser
  :<|> CreateBudget

type ApplyBudgetDeltaToUser =
  ReqBody '[JSON] Integer :> Patch '[JSON] BudgetResp

type CreateBudget =
  ReqBody '[JSON] CreateBudgetReqBody :> Post '[JSON] BudgetResp

data CreateBudgetReqBody = CreateBudgetReqBody
  { initialBudget :: Maybe Integer
  , lowerBound :: Maybe Integer
  } deriving (Generic, FromJSON, ToJSON)

data BudgetResp = BudgetResp
  { amount :: Integer
  , lowerBound :: Maybe Integer
  , isLowerBoundExceeded :: Bool
  } deriving (Generic, ToJSON, FromJSON)

