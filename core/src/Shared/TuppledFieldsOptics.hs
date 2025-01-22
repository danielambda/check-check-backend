{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Shared.TuppledFieldsOptics
  ( tuppledFields2
  , tuppledFields3, tuppledFields4
  , tuppledFields5, tuppledFields6
  , tuppledFields7, tuppledFields8
  , tuppledFields9, tuppledFields10
  , tuppledFields11, tuppledFields12
  , tuppledFields13, tuppledFields14
  ) where

import Optics (view, to)

tuppledFields2 field1 field2 = to $ (,)
  <$> view field1
  <*> view field2

tuppledFields3 field1 field2 field3 = to $ (,,)
  <$> view field1
  <*> view field2
  <*> view field3

tuppledFields4 field1 field2 field3 field4 = to $ (,,,)
  <$> view field1
  <*> view field2
  <*> view field3
  <*> view field4

-- I do not think I will use any of the below,
-- but it was fun to generate in a second them using vim
tuppledFields5 field1 field2 field3 field4 field5 = to $ (,,,,)
  <$> view field1
  <*> view field2
  <*> view field3
  <*> view field4
  <*> view field5

tuppledFields6 field1 field2 field3 field4 field5 field6 = to $ (,,,,,)
  <$> view field1
  <*> view field2
  <*> view field3
  <*> view field4
  <*> view field5
  <*> view field6

tuppledFields7 field1 field2 field3 field4 field5 field6 field7 = to $ (,,,,,,)
  <$> view field1
  <*> view field2
  <*> view field3
  <*> view field4
  <*> view field5
  <*> view field6
  <*> view field7

tuppledFields8 field1 field2 field3 field4 field5 field6 field7 field8 = to $ (,,,,,,,)
  <$> view field1
  <*> view field2
  <*> view field3
  <*> view field4
  <*> view field5
  <*> view field6
  <*> view field7
  <*> view field8

tuppledFields9 field1 field2 field3 field4 field5 field6 field7 field8 field9 = to $ (,,,,,,,,)
  <$> view field1
  <*> view field2
  <*> view field3
  <*> view field4
  <*> view field5
  <*> view field6
  <*> view field7
  <*> view field8
  <*> view field9

tuppledFields10 field1 field2 field3 field4 field5 field6 field7 field8 field9 field10 = to $ (,,,,,,,,,)
  <$> view field1
  <*> view field2
  <*> view field3
  <*> view field4
  <*> view field5
  <*> view field6
  <*> view field7
  <*> view field8
  <*> view field9
  <*> view field10

tuppledFields11 field1 field2 field3 field4 field5 field6 field7 field8 field9 field10 field11 = to $ (,,,,,,,,,,)
  <$> view field1
  <*> view field2
  <*> view field3
  <*> view field4
  <*> view field5
  <*> view field6
  <*> view field7
  <*> view field8
  <*> view field9
  <*> view field10
  <*> view field11

tuppledFields12 field1 field2 field3 field4 field5 field6 field7 field8 field9 field10 field11 field12 = to $ (,,,,,,,,,,,)
  <$> view field1
  <*> view field2
  <*> view field3
  <*> view field4
  <*> view field5
  <*> view field6
  <*> view field7
  <*> view field8
  <*> view field9
  <*> view field10
  <*> view field11
  <*> view field12

tuppledFields13 field1 field2 field3 field4 field5 field6 field7 field8 field9 field10 field11 field12 field13 = to $ (,,,,,,,,,,,,)
  <$> view field1
  <*> view field2
  <*> view field3
  <*> view field4
  <*> view field5
  <*> view field6
  <*> view field7
  <*> view field8
  <*> view field9
  <*> view field10
  <*> view field11
  <*> view field12
  <*> view field13

tuppledFields14 field1 field2 field3 field4 field5 field6 field7 field8 field9 field10 field11 field12 field13 field14 = to $ (,,,,,,,,,,,,,)
  <$> view field1
  <*> view field2
  <*> view field3
  <*> view field4
  <*> view field5
  <*> view field6
  <*> view field7
  <*> view field8
  <*> view field9
  <*> view field10
  <*> view field11
  <*> view field12
  <*> view field13
  <*> view field14
