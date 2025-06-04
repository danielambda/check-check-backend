{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}

module WebAPI.Budget (BudgetAPI, BudgetResp, Budget(..), budgetToResp, budgetServer) where

import Data.Aeson (ToJSON)
import Servant ((:<|>)((:<|>)), (:>), JSON, ReqBody, Get, Patch, ServerT)

import Control.Concurrent (readMVar, modifyMVar_)
import Control.Monad.Reader (asks, MonadIO (liftIO))
import GHC.Generics (Generic)

import WebAPI.AppM (AppM, Env (budgetAmountMVar))

type BudgetAPI = GetBudget :<|> ApplyBudgetDelta

type GetBudget = Get '[JSON] BudgetResp
type ApplyBudgetDelta = ReqBody '[JSON] Integer :> Patch '[JSON] BudgetResp

newtype BudgetResp = BudgetResp
  { amount :: Integer }
  deriving stock Generic
  deriving anyclass ToJSON

newtype Budget = Budget
  { amount :: Integer }

budgetServer :: ServerT BudgetAPI AppM
budgetServer = getBudget :<|> applyBudgetDelta

getBudget :: AppM BudgetResp
getBudget = do
  budgetAmountMVar <- asks budgetAmountMVar
  budgetAmount <- liftIO $ readMVar budgetAmountMVar
  let budget = Budget budgetAmount
  return $ budgetToResp budget

applyBudgetDelta :: Integer -> AppM BudgetResp
applyBudgetDelta delta = do
  budgetAmountMVar <- asks budgetAmountMVar
  budgetAmount <- liftIO $ do
    modifyMVar_ budgetAmountMVar (pure . (+delta))
    readMVar budgetAmountMVar
  let budget = Budget budgetAmount
  return $ budgetToResp budget

budgetToResp :: Budget -> BudgetResp
budgetToResp Budget{..} = BudgetResp{..}

