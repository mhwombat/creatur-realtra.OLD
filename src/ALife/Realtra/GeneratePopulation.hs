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
import ALife.Creatur.Wain.Statistics (stats)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (evalRandIO)
import Control.Monad.State.Lazy (StateT, evalStateT)

names :: [String]
names = map (("Founder" ++) . show) [1..Config.initialPopulationSize]

introduceRandomAgent :: String -> StateT (SimpleUniverse Astronomer) IO ()
introduceRandomAgent name = do
  agent <- liftIO $ evalRandIO (randomAstronomer name)
  writeToLog $ "GeneratePopulation: Created " ++ agentId agent
  writeToLog $ "GeneratePopulation: Stats " ++ show (stats agent)
  addAgent agent
  return ()
  
main :: IO ()
main = do
  evalStateT (mapM_ introduceRandomAgent names) Config.universe
