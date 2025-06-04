{-# LANGUAGE RecordWildCards #-}

module WebAPI.Budget (Budget(..), budgetToResp, budgetServer) where

import Servant ((:<|>)((:<|>)), ServerT)

import Control.Concurrent (readMVar, modifyMVar_)
import Control.Monad.Reader (asks, MonadIO (liftIO))

import WebAPI.AppM (AppM, Env (budgetAmountMVar))
import CheckCheck.Contracts.Budget (BudgetAPI, BudgetResp (..))

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

