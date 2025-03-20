{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot.FSA.Transitions.ShowReceipt (handleTransition) where

import Telegram.Bot.Simple (ReplyMessage (..), reply, actionButton)
import Telegram.Bot.API (InlineKeyboardMarkup(..), SomeReplyMarkup (..))
import Data.Text (Text)

import Data.Functor ((<&>))
import qualified Data.Text as T

import SmartPrimitives.Positive (Positive(..))
import CheckCheck.Contracts.Receipts (ReceiptResp(..), ReceiptItemResp (..))
import Clients.Utils (runReq)
import Clients.Backend (getReceipt)
import Models (ReceiptItem(..))
import Telegram.Bot.AppM ((<#), Eff', tg)
import Telegram.Bot.FSA
  ( State(InitialState)
  , Transition (SelectReceiptItem, StartSelectingReceiptItems, Id)
  )

handleTransition :: Text -> State -> Eff' Transition State
handleTransition qr InitialState = InitialState <# do
  ReceiptResp items <- runReq $ getReceipt qr
  let items' = process items
  let itemsButtons = items'
        <&> (:[])
         .  \(index, item) -> actionButton item (SelectReceiptItem index)
  let okButton = actionButton "Ok" Id
  let buttons = itemsButtons ++ [[okButton]]
  let msg' = "Scanned receipt items: "
        { replyMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup $
          InlineKeyboardMarkup buttons
        }
  tg $ reply msg'
  return $ StartSelectingReceiptItems qr $ items
    <&> \ReceiptItemResp{ price = Positive price, quantity = Positive quantity, .. } ->
      ReceiptItem{..}
  where
  process :: [ReceiptItemResp] -> [(Int, Text)]
  process = map $
    \ReceiptItemResp{ index, name, quantity = Positive quantity, price = Positive price } ->
      ( index
      , T.pack (show index)
        <> ". "
        <> T.take 20 name
        <> "... x "
        <> T.pack (show quantity)
        <> " = "
        <> T.pack (show (round (fromIntegral price * quantity) `divide` 100))
        <> " rub"
      )
    where
    divide :: Int -> Double -> Double
    divide a b = fromIntegral a / b
handleTransition _ _ = error "TODO"



