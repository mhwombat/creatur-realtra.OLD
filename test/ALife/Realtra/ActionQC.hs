------------------------------------------------------------------------
-- |
-- Module      :  ALife.Realtra.ActionQC
-- Copyright   :  (c) Amy de Buitléir 2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
module ALife.Realtra.ActionQC
  (
    test
  ) where

import ALife.Realtra.Action (Action)
import ALife.Realtra.TestUtils (prop_serialize_round_trippable,
  prop_genetic_round_trippable, prop_diploid_identity)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance Arbitrary Action where
  arbitrary = elements [minBound .. maxBound]

test :: Test
test = testGroup "ALife.Realtra.ActionQC"
  [
    testProperty "prop_serialize_round_trippable - Action"
      (prop_serialize_round_trippable :: Action -> Property),
    testProperty "prop_genetic_round_trippable - Action"
      (prop_genetic_round_trippable (==) :: Action -> Property),
    testProperty "prop_diploid_identity - Action"
      (prop_diploid_identity (==) :: Action -> Property)
  ]
