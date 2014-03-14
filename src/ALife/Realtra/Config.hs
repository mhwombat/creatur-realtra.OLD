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
import ALife.Realtra.ImageDB (ImageDB, mkImageDB)
import Data.Word (Word8, Word16)

universe :: SimpleUniverse a
universe = mkSimpleUniverse "GalaxyZoo" "/home/amy/alife/gzoo1" 100000

statsFile :: FilePath
statsFile = "/home/amy/alife/gzoo1/stats"

-- | Number of microseconds to sleep after each agent gets its turn at
--   the CPU.
sleepBetweenTasks :: Int
sleepBetweenTasks = 100

imageDB :: ImageDB
imageDB = mkImageDB "/home/amy/GalaxyZoo/table2/tiny-images"

imageHeight :: Int
imageHeight = 21
  
imageWidth :: Int
imageWidth = 21

-- | The classifier portion of a wain's brain is a Self-Organising Map
--   (SOM). This SOM uses a hexagonal grid with hexagonal tiles. The
--   setting below controls the maximum length of one side of the grid,
--   for the /initial/ population. The processing time required is
--   proportional to the square of this value.
initialPopulationMaxClassifierSize :: Word8
initialPopulationMaxClassifierSize = 5

-- | The decider portion of a wain's brain is also a SOM, using a
--   hexagonal grid with hexagonal tiles. The setting below controls the
--   maximum length of one side of the grid,
--   for the /initial/ population. The processing time
--   required is proportional to the square of this value.
initialPopulationMaxDeciderSize :: Word8
initialPopulationMaxDeciderSize = 5

-- | The maximum age at which wains mature in the initial population.
initialPopulationMaxAgeOfMaturity :: Word16
initialPopulationMaxAgeOfMaturity = 200

-- | The size of the initial population.
initialPopulationSize :: Int
initialPopulationSize = 100

-- | The maximum population size.
--   As the population increases toward this limit, the metabolism
--   cost increases, ensuring that only the fittest individuals
--   survive.
--   Note: It's unlikely the population will actually reach this limit
--   (because the metabolism costs will be so high), so set this value
--   a bit higher than your desired maximum population.
maxPopulationSize :: Int
maxPopulationSize = 500

-- | The maximum number of categories for classification
--   Note: It's unlikely the wains will actually reach this limit
--   (because the metabolism costs will be so high), so set this value
--   a bit higher than your desired maximum number of categories.
maxCategories :: Int
maxCategories = 100

--
-- Rewards and penalties
--

-- TODO MAKE CODE CONSISTENT WITH WHAT I WROTE IN THE THESIS.

-- *** Controlling the frequency of flirting

-- Every time an agent flirts, its energy changes by a fixed amount.
-- This is normally an energy LOSS, so it should be negative.
flirtingEnergyDelta :: Double
flirtingEnergyDelta = -0.01

-- Also see passionDelta

-- Note: Passion is reset to zero after mating.

-- *** Controlling the frequency of mating

-- Every time an agent mates, its energy changes by a fixed amount.
-- This is normally an energy LOSS, so it should be negative.
matingEnergyDelta :: Double
matingEnergyDelta = -0.01

-- If an agent is raising a child, then every time the parent gets a CPU
-- turn, its energy changes.
-- This is normally an energy LOSS, so it should be negative.
childRearingEnergyDelta :: Double
childRearingEnergyDelta = -0.005


-- Note: Passion is reset to zero after mating.

-- -- Every time an agent gets a CPU turn, its passion changes by a fixed
-- -- amount.
-- -- This is normally a passion GAIN, so it should be positive.
-- passionDelta :: Double
-- passionDelta = 0.1

-- *** Controlling an agent's CPU usage

-- Every time an agent gets a CPU turn, its energy changes based on how
-- its size.
-- Most of an agent's size is taken up by its brain, so the agent's size
-- can be used as a proxy for its CPU usage.
-- This is normally an energy LOSS, so it should be negative.
energyDeltaPerByte :: Double
energyDeltaPerByte = -0.000001

-- Every time an agent gets a CPU turn, its energy changes by an amount
-- based on normalised chi-squared (to discourage wasted categories)
-- This is normally an energy LOSS, so it should be negative.
conflationEnergyDeltaFactor :: Double
conflationEnergyDeltaFactor = -0.05

-- See also cooperationAgreementDelta

-- *** Controlling the frequency of co-operation

-- When an agent initiates co-operation (trading classifications), its
-- energy changes by a fixed amount.
-- This is normally an energy LOSS, so it should be negative.
cooperationEnergyDelta :: Double
cooperationEnergyDelta = -0.01

-- When two agents co-operate, and agree on a classification, their
-- energy changes by a fixed amount.
-- This is normally an energy GAIN, so it should be positive.
cooperationAgreementDelta :: Double
cooperationAgreementDelta = 1.0


