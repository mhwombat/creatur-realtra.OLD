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
import ALife.Realtra.Wain (Astronomer, randomAstronomer)
import qualified ALife.Realtra.Config as Config
import ALife.Creatur.Logger (writeToLog)
import ALife.Creatur.Universe (SimpleUniverse, addAgent)
-- import ALife.Creatur.Wain.Pretty (pretty)
-- import ALife.Creatur.Wain.Statistics (stats, maxStats, minStats, avgStats)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (evalRandIO)
import Control.Monad.State.Lazy (StateT, evalStateT)

-- buildAgents :: DiploidReader [Astronomer]
-- buildAgents = do  
--   agents <- mapM (buildWain True) names
--   return . catEithers $ agents

-- buildAgents :: RandomGen r => Rand r [Astronomer]
-- buildAgents = mapM randomAstronomer names

names :: [String]
names = map (("Founder" ++) . show) [1..Config.initialPopulationSize]

-- introduce :: Astronomer -> StateT (SimpleUniverse Astronomer) IO ()
-- introduce agent = do
--   writeToLog $ "GeneratePopulation: Created " ++ agentId agent
--   addAgent agent

-- introduceAll :: [Astronomer] -> StateT (SimpleUniverse Astronomer) IO ()
-- introduceAll agents = do
--   mapM_ introduce agents
--   let xs = map stats agents
--   writeToLog $ pretty (maxStats xs)
--   writeToLog $ pretty (minStats xs)
--   writeToLog $ pretty (avgStats xs)

introduceRandomAgent :: String -> StateT (SimpleUniverse Astronomer) IO ()
introduceRandomAgent name = do
  agent <- liftIO $ evalRandIO (randomAstronomer name)
  writeToLog $ "GeneratePopulation: Created " ++ agentId agent
  addAgent agent
  return ()
  
main :: IO ()
main = do
  -- r1 <- getStdGen -- source of random genes
  -- r2 <- getStdGen -- source of random genes

  -- let g1 = randoms r1
  -- let g2 = randoms r2

  -- let agents = runDiploidReader buildAgents (g1, g2)

  print names
  -- agents <- evalRandIO buildAgents
  -- evalStateT (introduceAll agents) Config.universe
  evalStateT (mapM_ introduceRandomAgent names) Config.universe

-- import ALife.Realtra.Wain
-- import Control.Monad.Random

-- main :: IO ()
-- main = do
--   fred <- evalRandIO $ randomAstronomer "fred"
--   print fred
