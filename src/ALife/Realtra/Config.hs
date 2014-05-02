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
{-# LANGUAGE TypeFamilies #-}
module ALife.Realtra.Config where

import ALife.Creatur.Universe (CachedUniverse, mkCachedUniverse)
import ALife.Realtra.ImageDB (mkImageDB)
import ALife.Realtra.Wain (Astronomer, Config(..))

onServer :: Bool
onServer = False

config :: Config (CachedUniverse Astronomer)
config = Config
  {
    universe
      = mkCachedUniverse
          "GalaxyZoo"             -- experiment name
          "/home/amy/alife/gzoo1" -- directory
          100000                  -- rotate log after this many records
          (if onServer            -- cache size (in bytes)
             then 20000000       
             else 10000000000),
    
    statsFile = "/home/amy/alife/gzoo1/stats",

    -- Number of microseconds to sleep after each agent gets its turn
    -- at the CPU.
    sleepBetweenTasks = 0,

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
    initialPopulationMaxAgeOfMaturity = if onServer then 100 else 2,

    -- The size of the initial population.
    initialPopulationSize = if onServer then 200 else 10,

    -- The maximum population size.
    -- As the population increases toward this limit, the metabolism
    -- cost increases, ensuring that only the fittest individuals
    -- survive.
    -- Note: It's unlikely the population will actually reach this limit
    -- (because the metabolism costs will be so high), so set this value
    -- a bit higher than your desired maximum population.
    maxPopulationSize = if onServer then 500 else 10,

    -- -- The minimum number of categories /actually used/.
    -- -- Note: It's unlikely the wains will actually this limit
    -- -- (because the metabolism costs will be so high), so set this value
    -- -- a bit lower than your desired minimum.
    -- minCategories = 2, -- really want at least 4

    -- The maximum number of categories /actually used/.
    -- Note: It's unlikely the wains will actually this limit
    -- (because the metabolism costs will be so high), so set this value
    -- a bit higher than your desired maximum.
    maxCategories = 50,

    -- The maximum agent size to allow.
    -- Most of an agent's size is taken up by its brain, so the agent's size
    -- can be used as a proxy for its CPU usage.
    -- Note: It's unlikely the wains will actually this limit
    -- (because the metabolism costs will be so high), so set this value
    -- a bit higher than your desired maximum.
    maxSize = 500000,

    -- To ensure that smaller agents don't have an excessive advantage
    -- over larger agents, part of the metabolic cost is fixed.
    -- Compare with maxSizeBasedMetabolismDeltaE.
    -- You probably won't need to alter this field.
    -- This is normally an energy LOSS, so it should be negative.
    baseMetabolismDeltaE = -0.05,

    -- To ensure that agents use resources efficiently, part of their
    -- metabolic cost is based on their size.
    -- You probably won't need to alter this field.
    -- This is normally an energy LOSS, so it should be negative.
    maxSizeBasedMetabolismDeltaE = -0.05,

    -- A wain rearing a child pays a fraction of the metabolic cost
    -- that the child would pay if it were full-grown. It's only a
    -- fraction because, biologically speaking, a child is only
    -- completely helpless when it's small (so the resource
    -- cost of raising it would be small).
    -- You probably won't need to alter this field.
    childCostFactor = 0.2,

    -- This must be an ODD integer >= 1. It is used to shape the
    -- reward/cost function for foraging. Higher values mean that
    -- rewards will be smaller except when the population is very near
    -- zero, and costs will be smaller except when the population is
    -- very near the maximum.
    -- You probably won't need to alter this field.
    foragingIndex = 3,
    
    --
    -- Rewards and penalties
    --

    -- TODO MAKE CODE CONSISTENT WITH WHAT I WROTE IN THE THESIS.

    -- *** Controlling the frequency of flirting

    -- Every time an agent flirts, its energy changes by a fixed amount.
    -- This is normally an energy LOSS, so it should be negative.
    flirtingDeltaE = -0.01,

    -- Note: Passion is reset to zero after mating.

    -- *** Controlling the frequency of mating

    -- Every time an agent mates, its energy changes by a fixed amount.
    -- This is normally an energy LOSS, so it should be negative.
    matingDeltaE = -0.01,

    -- Note: Passion is reset to zero after mating.

    -- *** Controlling the frequency of co-operation

    -- When an agent initiates co-operation (trading classifications), its
    -- energy changes by a fixed amount.
    -- This is normally an energy LOSS, so it should be negative.
    cooperationDeltaE = -0.01,

    -- When two agents co-operate, and agree on a classification, their
    -- energy changes by this amount, multiplied by the quality of
    -- the overall classification schema.
    -- This is normally an energy GAIN, so it should be positive.
    cooperationAgentAgreementDelta = 1.0,

    -- When two agents co-operate, and agree on a classification, their
    -- energy changes by this amount, multiplied by the quality of
    -- the overall classification schema.
    -- This is normally an energy GAIN, so it should be positive.
    cooperationImageAgreementDelta = 0.01
  }


