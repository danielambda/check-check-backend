module ReceiptItemAssociations.Types (ReceiptItemAssociation) where

import Groups.Types (GroupId)
import Goods.Types (GoodsId)

data ReceiptItemAssociation = ReceiptItemAssociation
  { groupId :: GroupId
  , receiptItemName :: String
  , goodsId :: GoodsId
  }
