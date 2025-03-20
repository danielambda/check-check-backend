{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot.FSA.Transitions.SelectReceiptItem (handleTransition) where

import Telegram.Bot.Simple (EditMessage(..), actionButton, editUpdateMessage)
import Telegram.Bot.API (InlineKeyboardMarkup(..), SomeReplyMarkup (..))
import Optics ((&), (%), unsafeFiltered, traversed, (^.), _1, _2, (<&>), (.~), view)
import qualified Data.Text as T

import Models (ReceiptItem(..))
import Telegram.Bot.AppM ((<#), tg, Eff')
import Telegram.Bot.FSA
  ( State(SelectingReceiptItems)
  , Transition (StartSelectingRequestRecipient, SelectReceiptItem, Id)
  )

handleTransition :: Int -> State -> Eff' Transition State
handleTransition i (SelectingReceiptItems qr items) =
  let items' = items & traversed % unsafeFiltered ((i ==) . (^. _2 % #index)) % _1 .~ True
  in SelectingReceiptItems qr items' <# do
    let confirmButton = actionButton "Confirm" StartSelectingRequestRecipient
    let cancelButton  = actionButton "Cancel"  Id
    let bottomButtonRow = [confirmButton, cancelButton]
    let itemsButtons = items'
          <&> (:[])
           .  \(_, item@ReceiptItem{index}) -> actionButton (toBtnText item) (SelectReceiptItem index)
    let buttons = itemsButtons ++ [bottomButtonRow]
    let editMsg = EditMessage
          { editMessageText = "Selected receipt items: "
              <> T.pack (show (view #index . snd <$> filter fst items'))
          , editMessageParseMode = Nothing
          , editMessageLinkPreviewOptions = Nothing
          , editMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup $
            InlineKeyboardMarkup buttons
          }
    tg $ editUpdateMessage editMsg
    where
    toBtnText ReceiptItem{ index, name, quantity, price }
      =  T.pack (show index)
      <> ". "
      <> T.take 20 name
      <> "... x "
      <> T.pack (show quantity)
      <> " = "
      <> T.pack (show (round (fromIntegral price * quantity) `divide` 100))
      <> " rub"
      where
      divide :: Int -> Double -> Double
      divide a b = fromIntegral a / b
handleTransition _ _ = error "TODO"
