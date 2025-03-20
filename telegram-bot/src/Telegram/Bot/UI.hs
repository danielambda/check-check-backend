{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Telegram.Bot.UI (tshow, messageWithButtons, toSelectReceiptItemButton) where

import Optics (view)
import Telegram.Bot.Simple
  ( actionButton
  , EditMessage (editMessageReplyMarkup), toEditMessage
  , ReplyMessage (replyMessageReplyMarkup), toReplyMessage
  )
import Telegram.Bot.API
  ( InlineKeyboardButton, InlineKeyboardMarkup (InlineKeyboardMarkup)
  , SomeReplyMarkup (SomeInlineKeyboardMarkup)
  )
import qualified Data.Text as T

import Control.Arrow ((&&&))

import Models (ReceiptItem(..))
import Telegram.Bot.FSA (Transition (SelectReceiptItem))

tshow :: Show a => a -> T.Text
tshow = T.pack . show

class MessageWithButtons a where
  messageWithButtons :: T.Text -> [[InlineKeyboardButton]] -> a

instance MessageWithButtons ReplyMessage where
  messageWithButtons txt buttons = (toReplyMessage txt)
    {replyMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup $ InlineKeyboardMarkup buttons}

instance MessageWithButtons EditMessage where
  messageWithButtons txt buttons = (toEditMessage txt)
    {editMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup $ InlineKeyboardMarkup buttons}

toSelectReceiptItemButton :: ReceiptItem -> InlineKeyboardButton
toSelectReceiptItemButton
  = (\(i, item) -> actionButton item (SelectReceiptItem i))
  . process
  where
  process = view #index &&& toBtnText
  toBtnText ReceiptItem{index, name, quantity, price}
    =  tshow index <> ". "
    <> T.take 20 name <> "... x "
    <> tshow quantity <> " = "
    <> tshow priceSum <> " rub"
    where

    priceSum :: Double
    priceSum = round (fromIntegral price * quantity) `divide` 100
      where
      divide :: Integer -> Double -> Double
      divide a b = fromIntegral a / b

