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

import ALife.Creatur (agentId)
import ALife.Realtra.Wain (Astronomer, randomAstronomer, summarise)
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.Statistics (Statistic, stats)
import qualified ALife.Realtra.Config as Config
import ALife.Creatur.Universe (writeToLog)
import ALife.Creatur.Universe (SimpleUniverse, store)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (evalRandIO)
import Control.Monad.Random.Class (getRandomR)
import Control.Monad.State.Lazy (StateT, evalStateT)

names :: [String]
names = map (("Founder" ++) . show) [1..Config.initialPopulationSize]

introduceRandomAgent
  :: String
    -> StateT (SimpleUniverse Astronomer) IO [Statistic]
introduceRandomAgent name = do
  classifierSize
    <- liftIO . evalRandIO $
        getRandomR (1,Config.initialPopulationMaxClassifierSize)
  deciderSize
    <- liftIO . evalRandIO $
        getRandomR (1,Config.initialPopulationMaxDeciderSize)
  agent
    <- liftIO $
        evalRandIO (randomAstronomer name classifierSize deciderSize)
  writeToLog $ "GeneratePopulation: Created " ++ agentId agent
  writeToLog $ "GeneratePopulation: Stats " ++ pretty (stats agent)
  store agent
  return (stats agent)

introduceRandomAgents
  :: [String] -> StateT (SimpleUniverse Astronomer) IO ()
introduceRandomAgents ns = do
  xs <- mapM introduceRandomAgent ns
  summarise xs
  
main :: IO ()
main = do
  print names
  evalStateT (introduceRandomAgents names) Config.universe
  
