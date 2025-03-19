{-# LANGUAGE TupleSections #-}

module Telegram.Bot.FSA.Transitions.StartSelectingReceiptItems (handleTransition) where

import Data.Text (Text)

import Telegram.Bot.FSA (State(SelectingReceiptItems))
import Models (ReceiptItem)

handleTransition :: Applicative f => Text -> [ReceiptItem] -> State -> f State
handleTransition qr items _ = pure $ SelectingReceiptItems qr (map (False,) items)
