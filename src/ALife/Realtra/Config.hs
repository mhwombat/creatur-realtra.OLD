------------------------------------------------------------------------
-- |
-- Module      :  ALife.Realtra.Config
-- Copyright   :  (c) Amy de Buitléir 2012-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Configuration parameters.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeOperators #-}
module ALife.Realtra.Config where

import ALife.Creatur.Universe (mkSimpleUniverse, SimpleUniverse)
import ALife.Creatur.Wain.UnitInterval (UIDouble(..))
import ALife.Realtra.ImageDB (ImageDB, mkImageDB)
import Data.Word (Word8, Word16)

--import Data.Array.Repa ((:.) (..), Z (..))

universe :: SimpleUniverse a
universe = mkSimpleUniverse "GalaxyZoo" "/home/amy/alife/gzoo1" 100000

-- imageSize :: ((Z :. Int) :. Int) :. Int
-- imageSize = Z :. (143 :: Int) :. (143 :: Int) :. (3 :: Int)

imageDB :: ImageDB
imageDB = mkImageDB "/home/amy/nosync/GalaxyZoo/table2/cropped-bw-images/"

imageHeight :: Int
imageHeight = 143
  
imageWidth :: Int
imageWidth = 143

-- | The classifier portion of a wain's brain is a Self-Organising Map
--   (SOM). This SOM uses a hexagonal grid with hexagonal tiles. The
--   setting below controls the maximum length of one side of the grid.
--   The processing time required is proportional to the square of this
--   value.
maxClassifierSize :: Word8
maxClassifierSize = 5

-- | The decider portion of a wain's brain is also a SOM, using a
--   hexagonal grid with hexagonal tiles. The setting below controls the
--   maximum length of one side of the grid. The processing time
--   required is proportional to the square of this value.
maxDeciderSize :: Word8
maxDeciderSize = 5

maxAgeOfMaturity :: Word16
maxAgeOfMaturity = 200

initialPopulationSize :: Int
initialPopulationSize = 10

--
-- Rewards and penalties
--

-- TODO MAKE CODE CONSISTENT WITH WHAT I WROTE IN THE THESIS.

-- *** Controlling the frequency of flirting

-- Every time an agent flirts, its energy changes by a fixed amount.
-- This is normally an energy LOSS, so it should be negative.
flirtingEnergyDelta :: UIDouble
flirtingEnergyDelta = -0.01

-- Also see passionDelta

-- Note: Passion is reset to zero after mating.

-- *** Controlling the frequency of mating

-- Every time an agent mates, its energy changes by a fixed amount.
-- This is normally an energy LOSS, so it should be negative.
matingEnergyDelta :: UIDouble
matingEnergyDelta = -0.01

-- If an agent is raising a child, then every time the parent gets a CPU
-- turn, its energy changes.
-- This is normally an energy LOSS, so it should be negative.
childRearingEnergyDelta :: UIDouble
childRearingEnergyDelta = -0.005


-- Note: Passion is reset to zero after mating.

-- Every time an agent gets a CPU turn, its passion changes by a fixed
-- amount.
-- This is normally a passion GAIN, so it should be positive.
passionDelta :: UIDouble
passionDelta = 0.1

-- *** Controlling the frequency of object classification

-- TODO WHY NOT JUST CLASSIFY ALL THE TIME? OR ONLY WHEN CONFIDENT???
-- TODO OR SAY SOMETHING ABOUT ONLY CLASSIFYING IMAGES, NOT AGENTS.

-- When an agent classifies an object, its energy changes by a fixed
-- amount.
-- This is normally an energy LOSS, so it should be negative.
classificationEnergyDelta :: Double
classificationEnergyDelta = -0.01

-- When an agent classifies an object, its energy changes based on how
-- many models it has.
-- This is normally an energy GAIN, so it should be positive.
classificationEnergyDeltaPerModel :: Double
classificationEnergyDeltaPerModel = 0.005

-- *** Controlling the size of the classifier

-- Every time an agent gets a CPU turn, its energy changes based on how
-- many classifier models it has.
-- This is normally an energy LOSS, so it should be negative.
energyDeltaPerClassifierModel :: UIDouble
energyDeltaPerClassifierModel = -0.001

-- Every time an agent gets a CPU turn, its energy changes by an amount
-- based on normalised chi-squared (to discourage wasted categories)
-- This is normally an energy LOSS, so it should be negative.
conflationEnergyDeltaFactor :: Double
conflationEnergyDeltaFactor = 0.001

-- See also classificationEnergyDeltaPerModel?????

-- See also cooperationAgreementDelta

-- *** Controlling the size of the decider

-- Every time an agent gets a CPU turn, its energy changes based on how
-- many decider models it has.
-- This is normally an energy LOSS, so it should be negative.
energyDeltaPerDeciderModel :: UIDouble
energyDeltaPerDeciderModel = -0.001

-- *** Controlling the frequency of co-operation

-- When an agent initiates co-operation (trading classifications), its
-- energy changes by a fixed amount.
-- This is normally an energy LOSS, so it should be negative.
cooperationEnergyDelta :: UIDouble
cooperationEnergyDelta = -0.1

-- When two agents co-operate, and agree on a classification, their
-- energy changes by a fixed amount.
-- This is normally an energy GAIN, so it should be positive.
cooperationAgreementDelta :: UIDouble
cooperationAgreementDelta = 0.3


