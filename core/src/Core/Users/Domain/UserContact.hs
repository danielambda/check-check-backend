{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Core.Users.Domain.UserContact (UserContact(..)) where

import Optics (makeFieldLabelsNoPrefix)

import Core.Users.Domain.UserId (UserId)
import Core.Users.Domain.UserType (UserType(Single))
import SmartPrimitives.TextMaxLen (TextMaxLen)

data UserContact = UserContact
  { contactUserId :: UserId 'Single
  , mContactName :: Maybe (TextMaxLen 50)
  }

makeFieldLabelsNoPrefix ''UserContact

