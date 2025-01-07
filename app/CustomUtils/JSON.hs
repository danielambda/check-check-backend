module CustomUtils.JSON ((*:)) where

import Data.Aeson ((.:), Key, Object, FromJSON)
import Data.Aeson.Types (Parser)

(*:) :: FromJSON a  => Parser Object -> Key -> Parser a
pObj *: key = pObj >>= (.: key)

