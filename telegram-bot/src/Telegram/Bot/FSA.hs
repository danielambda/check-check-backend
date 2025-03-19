{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Telegram.Bot.FSA
  ( State(..)
  , Transition(..)
  , ReceiptItem(..)
  ) where

import Data.Text (Text)
import Data.UUID (UUID)
import Optics (LabelOptic (..), A_Getter, to)

import Data.List.NonEmpty (NonEmpty)

data ReceiptItem = ReceiptItem
  { index :: Int
  , name :: Text
  , price :: Integer
  , quantity :: Double
  } deriving (Show, Read)

instance LabelOptic "index" A_Getter ReceiptItem ReceiptItem Int Int where
  labelOptic = to index
instance LabelOptic "name" A_Getter ReceiptItem ReceiptItem Text Text where
  labelOptic = to name

data State
  = InitialState
  | SelectingReceiptItems Text [(Bool, ReceiptItem)]
  | SelectingRequestRecipient Text (NonEmpty Int)
  deriving Show

data Transition
  = GetCurrentState
  | Id
  | Start
  | ShowReceipt Text
  | AddContact Text
  | StartSelectingReceiptItems Text [ReceiptItem]
  | SelectReceiptItem Int
  | StartSelectingRequestRecipient
  | SelectRequestRecipient UUID
  | CancelSelectingRequestRecipient
  deriving (Read, Show)

