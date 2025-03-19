{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot.FSA.Transitions.ShowReceipt (handleTransition) where

import Telegram.Bot.Simple (ReplyMessage (replyMessageReplyMarkup), reply, actionButton)

import Telegram.Bot.API (InlineKeyboardMarkup(..), SomeReplyMarkup (..))
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as T

import SmartPrimitives.Positive (Positive(..))
import CheckCheck.Contracts.Receipts (ReceiptResp(..), ReceiptItemResp (..))
import Clients.Utils (runReq)
import Clients.Backend (getReceipt)
import Models (ReceiptItem(..))
import Telegram.Bot.AppM ((<#), Eff', tg)
import Telegram.Bot.FSA
  ( State(InitialState)
  , Transition (StartSelectingRequestRecipient, SelectReceiptItem, StartSelectingReceiptItems)
  )

handleTransition :: Text -> State -> Eff' Transition State
handleTransition qr InitialState = InitialState <# do
  ReceiptResp items <- runReq $ getReceipt qr
  let items' = process items
  let confirmButton = actionButton "Confirm" StartSelectingRequestRecipient
  let itemsButtons = items'
        <&> (:[])
         .  \(index, item) -> actionButton item (SelectReceiptItem index)
  let buttons = itemsButtons ++ [[confirmButton]]
  let msg' = "Scanned receipt items: "
        { replyMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup $ InlineKeyboardMarkup
          { inlineKeyboardMarkupInlineKeyboard = buttons }
        }
  tg $ reply msg'
  return $ StartSelectingReceiptItems qr $ items
    <&> \ReceiptItemResp{ price = Positive price, quantity = Positive quantity, .. } ->
      ReceiptItem{..}
handleTransition _ _ = error "TODO"

process :: [ReceiptItemResp] -> [(Int, Text)]
process = fmap $
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

divide :: Int -> Double -> Double
divide a b = fromIntegral a / b

