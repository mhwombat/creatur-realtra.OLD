------------------------------------------------------------------------
-- |
-- Module      :  ALife.Realtra.Daemon
-- Copyright   :  (c) Amy de Buitléir 2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- The daemon that runs the Créatúr experiment.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Main where

import ALife.Creatur (programVersion)
import ALife.Creatur.Daemon (Daemon(..), launch)
import ALife.Creatur.Universe (Universe, Agent, writeToLog,
  replenishEnergyPool)
import ALife.Creatur.Task (simpleDaemon, runInteractingAgents)
import ALife.Creatur.Wain (programVersion)
import ALife.Realtra.Wain (Astronomer, universe, sleepBetweenTasks, run,
  finishRound, statsFile, minPopulationSize, maxPopulationSize,
  energyPoolSize)
import qualified ALife.Realtra.Config as Config
import Control.Monad.State (StateT, execStateT)
import Data.Version (showVersion)
import Paths_creatur_realtra (version)

startupHandler :: Universe u => String -> u -> IO u
startupHandler programName
  = execStateT (writeToLog $ "Starting " ++ programName)

shutdownHandler :: Universe u => String -> u -> IO ()
shutdownHandler programName u =
  execStateT (writeToLog $ "Shutdown requested for " ++ programName) u
  >> return ()

startRoundProgram :: (Universe u, Agent u ~ Astronomer) => StateT u IO ()
startRoundProgram = replenishEnergyPool (energyPoolSize Config.config)

endRoundProgram :: (Universe u, Agent u ~ Astronomer) => StateT u IO ()
endRoundProgram = finishRound (statsFile Config.config)

main :: IO ()
main = launch daemon (universe Config.config)
  where program = run Config.config
        popRange = ( minPopulationSize Config.config,
                     maxPopulationSize Config.config )
        daemon = simpleDaemon
                   {task=runInteractingAgents program popRange
                           startRoundProgram endRoundProgram,
                    onStartup=startupHandler message,
                    onShutdown=shutdownHandler message,
                    sleepTime=sleepBetweenTasks Config.config}
        message = "creatur-realtra-" ++ showVersion version
          ++ ", compiled with " ++ ALife.Creatur.Wain.programVersion
          ++ ", " ++ ALife.Creatur.programVersion
          ++ ", configuration=" ++ show Config.config
