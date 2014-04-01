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

config :: Config a
config = Config
  {
    universe
      = mkSimpleUniverse "GalaxyZoo" "/home/amy/alife/gzoo1" 100000,

    statsFile = "/home/amy/alife/gzoo1/stats",

    -- Number of microseconds to sleep after each agent gets its turn
    -- at the CPU.
    sleepBetweenTasks = 100,

    imageDB = mkImageDB "/home/amy/GalaxyZoo/table2/tiny-images",

    imageHeight = 21,
    imageWidth = 21,

    -- The classifier portion of a wain's brain is a Self-Organising Map
    -- (SOM). This SOM uses a hexagonal grid with hexagonal tiles. The
    -- setting below controls the maximum length of one side of the
    -- grid, for the /initial/ population. The processing time required
    -- is proportional to the square of this value.
    initialPopulationMaxClassifierSize = 5,

    -- The decider portion of a wain's brain is also a SOM, using a
    -- hexagonal grid with hexagonal tiles. The setting below controls
    -- the maximum length of one side of the grid,
    -- for the /initial/ population. The processing time
    -- required is proportional to the square of this value.
    initialPopulationMaxDeciderSize = 5,

    -- The maximum age at which wains mature in the initial population.
    initialPopulationMaxAgeOfMaturity = 5,

    -- The size of the initial population.
    initialPopulationSize = 5,

    -- The maximum population size.
    -- As the population increases toward this limit, the metabolism
    -- cost increases, ensuring that only the fittest individuals
    -- survive.
    -- Note: It's unlikely the population will actually reach this limit
    -- (because the metabolism costs will be so high), so set this value
    -- a bit higher than your desired maximum population.
    maxPopulation = 50,

    -- The minimum number of categories /actually used/.
    -- Note: It's unlikely the wains will actually this limit
    -- (because the metabolism costs will be so high), so set this value
    -- a bit lower than your desired minimum.
    minCategories = 2, -- really want at least 4

    -- The maximum number of categories /actually used/.
    -- Note: It's unlikely the wains will actually this limit
    -- (because the metabolism costs will be so high), so set this value
    -- a bit higher than your desired maximum.
    maxCategories = 12,

    -- The maximum agent size to allow.
    -- Most of an agent's size is taken up by its brain, so the agent's size
    -- can be used as a proxy for its CPU usage.
    -- Note: It's unlikely the wains will actually this limit
    -- (because the metabolism costs will be so high), so set this value
    -- a bit higher than your desired maximum.
    maxSize = 500000,

    --
    -- A wain rearing a child pays a fraction of the metabolic cost
    -- that the child would pay if it were full-grown. It's only a
    -- fraction because, biologically speaking, a child is only
    -- completely helpless when it's small (so the resource
    -- cost of raising it would be small).
    --
    childCostFactor = 0.2,
    
    --
    -- Rewards and penalties
    --

    -- TODO MAKE CODE CONSISTENT WITH WHAT I WROTE IN THE THESIS.

    -- *** Controlling the frequency of flirting

    -- Every time an agent flirts, its energy changes by a fixed amount.
    -- This is normally an energy LOSS, so it should be negative.
    flirtingEnergyDelta = -0.01,

    -- Also see passionDelta

    -- Note: Passion is reset to zero after mating.

    -- *** Controlling the frequency of mating

    -- Every time an agent mates, its energy changes by a fixed amount.
    -- This is normally an energy LOSS, so it should be negative.
    matingEnergyDelta = -0.01,

    -- Note: Passion is reset to zero after mating.

    -- *** Controlling the frequency of co-operation

    -- When an agent initiates co-operation (trading classifications), its
    -- energy changes by a fixed amount.
    -- This is normally an energy LOSS, so it should be negative.
    cooperationEnergyDelta = -0.01,

    -- When two agents co-operate, and agree on a classification, their
    -- energy changes by this amount, multiplied by the quality of
    -- the overall classification schema.
    -- This is normally an energy GAIN, so it should be positive.
    cooperationAgreementDelta = 1.0
  }

data Config a = Config
  { universe :: SimpleUniverse a,
    statsFile :: FilePath,
    sleepBetweenTasks :: Int,
    imageDB :: ImageDB,
    imageHeight :: Int,
    imageWidth :: Int,
    initialPopulationMaxClassifierSize :: Word8,
    initialPopulationMaxDeciderSize :: Word8,
    initialPopulationMaxAgeOfMaturity :: Word16,
    initialPopulationSize :: Int,
    childCostFactor :: Double,
    maxPopulation :: Int,
    minCategories :: Int,
    maxCategories :: Int,
    maxSize :: Int,
    flirtingEnergyDelta :: Double,
    matingEnergyDelta :: Double,
    cooperationEnergyDelta :: Double,
    cooperationAgreementDelta :: Double
  } deriving (Show, Eq)

