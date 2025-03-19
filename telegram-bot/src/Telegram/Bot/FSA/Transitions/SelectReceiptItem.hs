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
  , Transition (StartSelectingRequestRecipient, SelectReceiptItem)
  )

handleTransition :: Int -> State -> Eff' Transition State
handleTransition i (SelectingReceiptItems qr items) =
  let items' = items & traversed % unsafeFiltered ((i ==) . (^. _2 % #index)) % _1 .~ True
  in SelectingReceiptItems qr items' <# do
    let confirmButton = actionButton "Confirm" StartSelectingRequestRecipient
    let itemsButtons = items'
          <&> (:[])
           .  \(_, ReceiptItem{ index, name }) -> actionButton name (SelectReceiptItem index)
    let buttons = itemsButtons ++ [[confirmButton]]
    let editMessage' = EditMessage
          { editMessageText = "selected items: " <> T.pack (show (view #index . snd <$> filter fst items'))
          , editMessageParseMode = Nothing
          , editMessageLinkPreviewOptions = Nothing
          , editMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup $ InlineKeyboardMarkup
            { inlineKeyboardMarkupInlineKeyboard = buttons }
          }
    tg $ editUpdateMessage editMessage'
handleTransition _ _ = error "TODO"
