{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot.FSA.Transitions.ShowReceipt (handleTransition) where

import Telegram.Bot.Simple (reply, actionButton)
import Data.Text (Text)

import CheckCheck.Contracts.Receipts (ReceiptResp(ReceiptResp))
import Clients.Utils (runReq)
import Clients.Backend (getReceipt)
import Models (FromResp (fromResp))
import Telegram.Bot.AppM ((<#), Eff', tg)
import Telegram.Bot.UI (toSelectReceiptItemButton, messageWithButtons)
import Telegram.Bot.FSA
  ( State(InitialState)
  , Transition (StartSelectingReceiptItems, Id)
  )

handleTransition :: Text -> State -> Eff' Transition State
handleTransition qr InitialState = InitialState <# do
  ReceiptResp respItems <- runReq $ getReceipt qr
  let items = fromResp <$> respItems
  let itemsButtons = (:[]) . toSelectReceiptItemButton <$> items
  let okButton = actionButton "Ok" Id
  let buttons = itemsButtons ++ [[okButton]]
  let msg = messageWithButtons "Scanned receipt items: " buttons
  tg $ reply msg
  return $ StartSelectingReceiptItems qr items

handleTransition _ _ = error "TODO"
