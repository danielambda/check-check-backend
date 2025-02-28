{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}

-- Just for some fun
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module WebAPI.Users.Get
  ( Dependencies
  , GetUser, getUser
  , UserSingleResp(..), BudgetResp(..), toResp
  ) where

import Servant (ServerT, (:>), JSON, Get, Capture, throwError, err404, ServerError)
import Data.Aeson (ToJSON)
import Data.UUID (UUID)
import Optics ((^?), (%), to, (<&>), (&), (^.))

import GHC.Generics (Generic)

import SmartPrimitives.TextLenRange (TextLenRange)
import Core.Common.Operators ((^^.), (^^?))
import Core.Users.Budget.Domain.Budget (Budget, BudgetLowerBoundStatus (BudgetLowerBoundExceeded))
import Core.Users.Domain.UserType (UserType(Single))
import Core.Users.Domain.User (User)
import Core.Users.Domain.UserId (UserId(UserId))
import qualified Core.Users.Get as Impl (get, Dependencies)

import Control.Monad.Error.Class (MonadError)

type GetUser =
  Capture "userId" UUID :> Get '[JSON] UserSingleResp

data UserSingleResp = UserSingleResp
  { userId :: UUID
  , username :: TextLenRange 2 50
  , budget :: Maybe BudgetResp
  } deriving (Generic, ToJSON)

data BudgetResp = BudgetResp
  { amount :: Integer
  , lowerBound :: Maybe Integer
  , isLowerBoundExceeded :: Bool
  } deriving (Generic, ToJSON)

type Dependencies m = (Impl.Dependencies m, MonadError ServerError m)
getUser ::  Dependencies m => ServerT GetUser m
getUser userId = userId
   &  UserId
   &  Impl.get
  <&> fmap toResp
  >>= maybe (throwError err404) return

toResp' :: User 'Single -> UserSingleResp
toResp :: User 'Single -> UserSingleResp
toResp' user = UserSingleResp
  { userId = user ^^. #userId
  , username = user ^^. #data % #username
  , budget = user ^? #data % #mBudget % to budgetResp
  } where  -- I know this syntax looks cursed. That's why I like it.
    budgetResp :: Budget -> BudgetResp
    budgetResp budget = BudgetResp
      { amount = budget ^^. #amount
      , lowerBound = budget ^^? #mLowerBound
      , isLowerBoundExceeded = budget ^. #lowerBoundStatus == BudgetLowerBoundExceeded
      }

-- Загадка от Жака Фреско: Why GHC does not curse me even tho here is no type signature
toResp = UserSingleResp
  <$> (^^. #userId)
  <*> (^^. #data % #username)
  <*> (^? #data % #mBudget % to (BudgetResp
    <$> (^^. #amount)
    <*> (^^? #mLowerBound)
    <*> (BudgetLowerBoundExceeded ==) . (^. #lowerBoundStatus)
    )
  )
