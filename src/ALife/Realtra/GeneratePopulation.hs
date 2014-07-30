------------------------------------------------------------------------
-- |
-- Module      :  ALife.Realtra.GeneratePopulation
-- Copyright   :  (c) Amy de Buitléir 2012-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}

import ALife.Creatur (agentId)
import ALife.Realtra.Wain (Astronomer, randomAstronomer, summarise,
  initialPopulationSize, initialPopulationMaxClassifierSize,
  initialPopulationMaxDeciderSize, universe)
import ALife.Creatur.Wain (adjustEnergy)
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.Statistics (Statistic, stats)
import qualified ALife.Realtra.Config as Config
import ALife.Creatur.Universe (Universe, Agent, writeToLog, store)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (evalRandIO)
import Control.Monad.Random.Class (getRandomR)
import Control.Monad.State.Lazy (StateT, evalStateT)

names :: [String]
names = map (("Founder" ++) . show)
          [1..(initialPopulationSize Config.config)]

introduceRandomAgent
  :: (Universe u, Agent u ~ Astronomer)
     => String -> StateT u IO [Statistic]
introduceRandomAgent name = do
  classifierSize
    <- liftIO . evalRandIO $
        getRandomR (1, initialPopulationMaxClassifierSize Config.config)
  deciderSize
    <- liftIO . evalRandIO $
        getRandomR (1, initialPopulationMaxDeciderSize Config.config)
  -- Make the first generation a little hungry so they start learning
  -- immediately.
  agent
    <- fmap (adjustEnergy (-0.1)) . liftIO $
        evalRandIO ( randomAstronomer name Config.config classifierSize
                     deciderSize )
  writeToLog $ "GeneratePopulation: Created " ++ agentId agent
  writeToLog $ "GeneratePopulation: Stats " ++ pretty (stats agent)
  store agent
  return (stats agent)

introduceRandomAgents
  :: (Universe u, Agent u ~ Astronomer)
     => [String] -> StateT u IO ()
introduceRandomAgents ns = do
  xs <- mapM introduceRandomAgent ns
  summarise xs
  
main :: IO ()
main = do
  print names
  evalStateT (introduceRandomAgents names) (universe Config.config)
  
