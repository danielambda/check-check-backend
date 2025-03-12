{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Core.Users.Domain.UserContact (UserContact(..)) where

import Optics (makeFieldLabelsNoPrefix)

import SmartPrimitives.TextMaxLen (TextMaxLen)
import Core.Users.Domain.UserId (UserId)
import Core.Users.Domain.UserType (UserType(Single))
import Core.Users.Domain.Primitives (Username)

data UserContact = UserContact
  { contactUserId :: UserId 'Single
  , username :: Username
  , contactName :: Maybe (TextMaxLen 50)
  }

makeFieldLabelsNoPrefix ''UserContact

