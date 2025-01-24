{-# LANGUAGE DataKinds #-}

module Core.Groups.Domain.Primitives (GroupName (GroupName)) where

import SmartPrimitives.TextLenRange (TextLenRange)

newtype GroupName = GroupName (TextLenRange 2 50)
