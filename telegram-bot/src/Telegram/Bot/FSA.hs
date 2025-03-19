{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Telegram.Bot.FSA
  ( State(..)
  , Transition(..)
  ) where

import Data.Text (Text)
import Data.UUID (UUID)

import Data.List.NonEmpty (NonEmpty)

import Models (ReceiptItem(..))

data State
  = InitialState
  | SelectingReceiptItems Text [(Bool, ReceiptItem)]
  | SelectingRequestRecipient Text (NonEmpty Int)
  deriving Show

data Transition
  = Id
  | Start
  | ShowReceipt Text
  | AddContact Text
  | StartSelectingReceiptItems Text [ReceiptItem]
  | SelectReceiptItem Int
  | StartSelectingRequestRecipient
  | SelectRequestRecipient UUID
  | CancelSelectingRequestRecipient
  deriving (Read, Show)

