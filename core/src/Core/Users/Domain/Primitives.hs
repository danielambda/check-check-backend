{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Core.Users.Domain.Primitives (Username(..)) where

import Optics (makeFieldLabelsNoPrefix)

import SmartPrimitives.TextLenRange (TextLenRange)

newtype Username = Username { value :: TextLenRange 2 50 }

makeFieldLabelsNoPrefix ''Username
