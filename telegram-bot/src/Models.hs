{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Models (UserContact(..), ReceiptItem(..)) where

import Data.UUID (UUID)

import SmartPrimitives.TextLenRange (TextLenRange)
import SmartPrimitives.TextMaxLen (TextMaxLen)
import Data.Text (Text)
import Optics (LabelOptic (labelOptic), A_Getter, to)

data UserContact = UserContact
  { contactUserId :: UUID
  , contactUsername :: TextLenRange 2 50
  , mContactName :: Maybe (TextMaxLen 50)
  }

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

