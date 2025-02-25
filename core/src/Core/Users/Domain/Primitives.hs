{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Core.Users.Domain.Primitives (Username(..)) where

import Optics (A_Lens, LabelOptic (labelOptic), lensVL, (<&>))

import SmartPrimitives.TextLenRange (TextLenRange)

newtype Username = Username { value :: TextLenRange 2 50 }

instance (a ~ TextLenRange 2 50, a ~ b) => LabelOptic "value" A_Lens Username Username a b where
  labelOptic = lensVL $ \f (Username username) ->
    f username <&> Username
