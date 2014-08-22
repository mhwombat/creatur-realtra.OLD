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
          (if onServer            -- cache size (in bytes)
             then 10000000000
             else 20000000),

    statsFile = "/home/amy/alife/gzoo1/stats",
    rawStatsFile = "/home/amy/alife/gzoo1/rawStats",

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
    initialPopulationMaxClassifierSize = if onServer then 5 else 3,

    -- The decider portion of a wain's brain is also a SOM, using a
    -- hexagonal grid with hexagonal tiles. The setting below controls
    -- the maximum length of one side of the grid,
    -- for the /initial/ population. The processing time
    -- required is proportional to the square of this value.
    initialPopulationMaxDeciderSize = 5,

    -- The maximum age at which wains mature in the initial population.
    initialPopulationMaxAgeOfMaturity = if onServer then 100 else 2,

    -- The size of the initial population.
    initialPopulationSize = if onServer then 500 else 20,

    energyPoolSize = if onServer then 30 else 5,

    -- The daemon will stop if the population falls below this amount.
    -- This gives you a chance to analyse the problem and perhaps
    -- adjust your configuration.
    minPopulationSize = if onServer then 150 else 2,

    -- The daemon will stop if the population rises above this amount.
    -- This gives you a chance to analyse the problem and perhaps
    -- adjust your configuration.
    maxPopulationSize = if onServer then 800 else 20,

    -- -- The maximum number of categories /actually used/.
    -- -- Note: It's unlikely the wains will actually this limit
    -- -- (because the metabolism costs will be so high), so set this value
    -- -- a bit higher than your desired maximum.
    -- maxCategories = 50,

    -- To ensure that smaller agents don't have an excessive advantage
    -- over larger agents, part of the metabolic cost is fixed.
    -- Compare with maxSizeBasedMetabolismDeltaE.
    -- This is normally an energy LOSS, so it should be negative.
    baseMetabolismDeltaE = -0.05,

    -- To ensure that agents use resources efficiently, part of their
    -- metabolic cost is based on their size.
    -- If you don't know what value to use, multiply the average size
    -- of your initial population by -0.001 and use that.
    -- This is normally an energy LOSS, so it should be negative.
    energyCostPerByte = -0.0000005,

    -- A wain rearing a child pays a fraction of the metabolic cost
    -- that the child would pay if it were full-grown. It's only a
    -- fraction because, biologically speaking, a child is only
    -- completely helpless when it's small (so the resource
    -- cost of raising it would be small).
    -- You probably won't need to alter this field.
    childCostFactor = 0.2,

    easementTime = 500,
    easementCooperationBonus = 0.05,
    easementAgreementBonus = 0.25,

    -- *** Controlling the frequency of flirting

    -- Every time an agent flirts, its energy changes by a fixed amount.
    -- This is normally an energy LOSS, so it should be negative.
    flirtingDeltaE = -0.05,

    -- Note: Passion is reset to zero after mating.

    -- *** Controlling the frequency of mating

    -- Every time an agent mates, its energy changes by a fixed amount.
    -- This is normally an energy LOSS, so it should be negative.
    matingDeltaE = -0.05,

    -- Note: Passion is reset to zero after mating.

    -- *** Controlling the frequency of co-operation

    -- When an agent initiates co-operation (trading classifications), its
    -- energy changes by a fixed amount.
    -- This is normally an energy LOSS, so it should be negative.
    cooperationDeltaE = -0.01,

    -- When two agents co-operate, and agree on a classification,
    -- their energy changes by this amount, multiplied by the
    -- quality of the overall classification schema.
    -- This is normally an energy GAIN, so it should be positive.
    cooperationAgreementDelta = 1.0,

    -- The range of values allowed for r0 (the learning rate applied to
    -- the BMU at time 0) in the learning function for the
    -- classifier in the initial population.
    classifierR0Range = (0, 1),

    -- The range of values allowed for rf (the learning rate applied to
    -- the BMU at time tf) in the learning function for the
    -- classifier in the initial population.
    classifierRfRange = (0, 1),

    -- The range of values allowed for w0 (the neighbourhood width at
    -- time 0) in the learning function for the
    -- classifier in the initial population.
    classifierW0Range = (0, 5),

    -- The range of values allowed for wf (the neighbourhood width at
    -- time tf) in the learning function for the
    -- classifier in the initial population.
    classifierWfRange = (0, 5),

    -- The range of values allowed for tf (the time at which the learning
    -- rate becomes negligible) in the learning function for the
    -- classifier in the initial population.
    classifierTfRange = (1, 10000),

    -- The range of values allowed for r0 (the learning rate applied to
    -- the BMU at time 0) in the learning function for the
    -- decider in the initial population.
    deciderR0Range = (0, 1),

    -- The range of values allowed for rf (the learning rate applied to
    -- the BMU at time tf) in the learning function for the
    -- decider in the initial population.
    deciderRfRange = (0, 1),

    -- The range of values allowed for w0 (the neighbourhood width at
    -- time 0) in the learning function for the
    -- decider in the initial population.
    deciderW0Range = (0, 5),

    -- The range of values allowed for wf (the neighbourhood width at
    -- time tf) in the learning function for the
    -- decider in the initial population.
    deciderWfRange = (0, 5),

    -- The range of values allowed for tf (the time at which the learning
    -- rate becomes negligible) in the learning function for the
    -- decider in the initial population.
    deciderTfRange = (1, 10000)
  }
