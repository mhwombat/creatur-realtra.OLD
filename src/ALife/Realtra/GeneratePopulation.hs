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
  initialPopulationMaxDeciderSize, universe,
  initialPopulationMaxAgeOfMaturity, imageWidth, imageHeight)
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.Statistics (Statistic, stats)
import qualified ALife.Realtra.Config as Config
import ALife.Creatur.Universe (writeToLog)
import ALife.Creatur.Universe (CachedUniverse, store)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (evalRandIO)
import Control.Monad.Random.Class (getRandomR)
import Control.Monad.State.Lazy (StateT, evalStateT)

names :: [String]
names = map (("Founder" ++) . show)
          [1..(initialPopulationSize Config.config)]

introduceRandomAgent
  :: String -> StateT (CachedUniverse Astronomer) IO [Statistic]
introduceRandomAgent name = do
  classifierSize
    <- liftIO . evalRandIO $
        getRandomR (1, initialPopulationMaxClassifierSize Config.config)
  deciderSize
    <- liftIO . evalRandIO $
        getRandomR (1, initialPopulationMaxDeciderSize Config.config)
  let w = imageWidth Config.config
  let h = imageHeight Config.config
  let mm = initialPopulationMaxAgeOfMaturity Config.config
  agent
    <- liftIO $
        evalRandIO (randomAstronomer name w h classifierSize deciderSize mm)
  writeToLog $ "GeneratePopulation: Created " ++ agentId agent
  writeToLog $ "GeneratePopulation: Stats " ++ pretty (stats agent)
  store agent
  return (stats agent)

introduceRandomAgents
  :: [String] -> StateT (CachedUniverse Astronomer) IO ()
introduceRandomAgents ns = do
  xs <- mapM introduceRandomAgent ns
  summarise xs
  
main :: IO ()
main = do
  print names
  evalStateT (introduceRandomAgents names) (universe Config.config)
  
