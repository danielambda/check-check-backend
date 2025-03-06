{-# LANGUAGE
  DataKinds, TypeOperators, DeriveGeneric, DeriveAnyClass
#-}

module CheckCheck.Contracts.Receipts
  ( ReceiptsAPI
  , GetReceipt
  , ReceiptResp(..)
  , ReceiptItemResp(..)
  ) where

import Servant.API (Capture, (:>), Get, JSON)
import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)
import SmartPrimitives.Positive (Positive)
import GHC.Generics (Generic)

type ReceiptsAPI = GetReceipt

type GetReceipt =
  Capture "qr" Text :> Get '[JSON] ReceiptResp

newtype ReceiptResp = ReceiptResp
 { items :: [ReceiptItemResp]
 } deriving (Generic, ToJSON, FromJSON)

data ReceiptItemResp = ReceiptItemResp
  { index :: Int
  , name :: Text
  , quantity :: Positive Double
  , price :: Positive Integer
  } deriving (Generic, ToJSON, FromJSON)

