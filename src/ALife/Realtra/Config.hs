------------------------------------------------------------------------
-- |
-- Module      :  ALife.Realtra.Config
-- Copyright   :  (c) Amy de Buitléir 2012-2014
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
          "Mnist"                  -- experiment name
          "/home/amy/alife/mnist1" -- directory
          (if onServer             -- cache size (in bytes)
             then 10000000000
             else 20000000),

    statsFile = "/home/amy/alife/mnist1/stats",
    rawStatsFile = "/home/amy/alife/mnist1/rawStats",

    -- Number of microseconds to sleep after each agent gets its turn
    -- at the CPU.
    sleepBetweenTasks = 0,

    imageDB = mkImageDB "/home/amy/mnist/testDataWithAnomalies",

    imageHeight = 28,
    imageWidth = 28,

    -- The classifier portion of a wain's brain is a Self-Organising Map
    -- (SOM). This SOM uses a hexagonal grid with hexagonal tiles. The
    -- setting below controls the maximum length of one side of the
    -- grid, for the /initial/ population. The processing time required
    -- is proportional to the square of this value.
    initialPopulationClassifierSizeRange =
      if onServer then (2, 5) else (2, 3),

    -- The decider portion of a wain's brain is also a SOM, using a
    -- hexagonal grid with hexagonal tiles. The setting below controls
    -- the maximum length of one side of the grid,
    -- for the /initial/ population. The processing time
    -- required is proportional to the square of this value.
    initialPopulationDeciderSizeRange = (2, 5),

    -- The maximum amount of energy that wains in the initial population
    -- give to their children at birth.
    initialPopulationDevotionRange = (0, 0.3),

    -- The maximum age at which wains mature in the initial population.
    initialPopulationMaturityRange =
      if onServer then (1, 50) else (1, 2),

    -- The size of the initial population.
    initialPopulationSize = if onServer then 200 else 20,

    easementTime = 250,
    easementCooperationBonus = 0.1,
    easementAgreementBonus = 0.9,

    energyPoolSize = 1000,

    -- The daemon will stop if the population falls outside this range.
    -- This gives you a chance to analyse the problem and perhaps
    -- adjust your configuration.
    populationSizeRange = if onServer then (100, 300) else (4,20),

    -- To ensure that smaller agents don't have an excessive advantage
    -- over larger agents, part of the metabolic cost is fixed.
    -- Compare with maxSizeBasedMetabolismDeltaE.
    -- This is normally an energy LOSS, so it should be negative.
    -- You probably won't need to alter this field.
    baseMetabolismDeltaE = -0.005,

    -- To ensure that agents use resources efficiently, part of their
    -- metabolic cost is based on their size.
    -- If you don't know what value to use, multiply the average size
    -- of your initial population by -0.001 and use that.
    -- This is normally an energy LOSS, so it should be negative.
    energyCostPerByte = -0.00000001,

    -- A wain rearing a child pays a fraction of the metabolic cost
    -- that the child would pay if it were full-grown. It's only a
    -- fraction because, biologically speaking, a child is only
    -- completely helpless when it's small (so the resource
    -- cost of raising it would be small).
    -- You probably won't need to alter this field.
    childCostFactor = 0.2,

    -- *** Controlling the frequency of flirting

    -- Every time an agent flirts, its energy changes by a fixed amount.
    -- This is normally an energy LOSS, so it should be negative.
    flirtingDeltaE = -0.05,

    -- Note: Passion is reset to zero after mating.

    -- *** Controlling the frequency of co-operation

    -- When an agent initiates co-operation (trading classifications), its
    -- energy changes by a fixed amount.
    -- This is normally an energy LOSS, so it should be negative.
    cooperationDeltaE = -0.01,

    -- When two agents co-operate, and agree on a classification,
    -- their energy changes by this amount, multiplied by the
    -- novelty of the image.
    -- This is in addition to minimum agreement energy change.
    -- This is normally an energy GAIN, so it should be positive.
    noveltyBasedCooperationAgreementDeltaE = 0.9,

    -- When two agents co-operate, and agree on a classification,
    -- their energy changes by this amount.
    -- This is in addition to the energy change based on novelty.
    -- This is normally an energy GAIN, so it should be positive.
    minCooperationAgreementDeltaE = 0.1,

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
