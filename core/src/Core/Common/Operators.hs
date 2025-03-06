{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}

module Core.Common.Operators ((*>>), (.>), (^^.), (^^?), (^^..)) where

import Optics (A_Getter, Is, (^.), (%), Optic, LabelOptic, JoinKinds, An_AffineFold, (^?), A_Fold, (^..))

infixl 2 *>>
(*>>) :: (Applicative f1, Applicative f2) => f1 (f2 a) -> f1 (f2 b) -> f1 (f2 b)
(*>>) = liftA2 (*>)

infixl 9 .>
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

infixl 8 ^^.
(^^.) :: (JoinKinds optic labelOptic k, LabelOptic "value" labelOptic a b c c, Is k A_Getter)
      => s -> Optic optic ix s s a b -> c
s ^^. optic = s ^. optic % #value

infixl 8 ^^?
(^^?) :: (JoinKinds optic labelOptic k, LabelOptic "value" labelOptic a b c c, Is k An_AffineFold)
      => s -> Optic optic ix s s a b -> Maybe c
s ^^? optic = s ^? optic % #value

infix 8 ^^..
(^^..) :: (JoinKinds optic labelOptic k, LabelOptic "value" labelOptic a b c c, Is k A_Fold)
       => s -> Optic optic ix s s a b -> [c]
s ^^.. optic = s ^.. optic % #value
