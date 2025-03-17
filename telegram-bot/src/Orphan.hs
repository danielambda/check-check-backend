{-# LANGUAGE LambdaCase, MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphan () where

import Telegram.Bot.Simple (GetAction(getNextAction))

instance (GetAction a action, GetAction b action) => GetAction (Either a b) action where
  getNextAction bot = bot >>= \case
    Right a -> getNextAction $ pure a
    Left b -> getNextAction $ pure b

