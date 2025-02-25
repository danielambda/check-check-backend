{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module WebAPI.Users.CreateSingle
  ( Impl.Dependencies
  , CreateUserSingleReqBody(..)
  , CreateUserSingle, createUserSingle
  , UserSingleResp(..), BudgetResp(..)
  ) where

import Servant (ServerT, (:>), JSON, Post, ReqBody)
import Data.Aeson (FromJSON)

import GHC.Generics (Generic)

import SmartPrimitives.TextLenRange (TextLenRange)
import qualified Core.Users.CreateSingle as Impl (createSingle, Dependencies, Data(..))
import qualified Core.Users.Budget.Create as Impl.Budget (Data(..))
import WebAPI.Users.Get (UserSingleResp(..), BudgetResp(..), toResp)

type CreateUserSingle =
  ReqBody '[JSON] CreateUserSingleReqBody :> Post '[JSON] UserSingleResp

data CreateUserSingleReqBody = CreateUserSingleReqBody
  { name :: TextLenRange 2 50
  , budget :: Maybe CreateBudgetReqBody
  } deriving (Generic, FromJSON)

data CreateBudgetReqBody = CreateBudgetReqBody
  { initialAmount :: Maybe Integer
  , lowerBound :: Maybe Integer
  } deriving (Generic, FromJSON)

createUserSingle :: Impl.Dependencies m => ServerT CreateUserSingle m
createUserSingle CreateUserSingleReqBody{ name, budget } = do
  let mBudgetData = mkBudget <$> budget
  toResp <$> Impl.createSingle Impl.Data{..}
  where
    mkBudget CreateBudgetReqBody{ initialAmount, lowerBound } =
      Impl.Budget.Data { mInitialAmount = initialAmount
                       , mLowerBound = lowerBound
                       }

