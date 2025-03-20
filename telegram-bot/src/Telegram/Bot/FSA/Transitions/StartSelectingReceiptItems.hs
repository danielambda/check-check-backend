{-# LANGUAGE TupleSections #-}

module Telegram.Bot.FSA.Transitions.StartSelectingReceiptItems (handleTransition) where

import Telegram.Bot.FSA (State(SelectingReceiptItems, ViewingReceipt))

handleTransition :: Applicative f => State -> f State
handleTransition (ViewingReceipt qr items) = pure $ SelectingReceiptItems qr (map (False,) items)
handleTransition _ = error "TODO"
