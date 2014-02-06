------------------------------------------------------------------------
-- |
-- Module      :  ALife.Realtra.Daemon
-- Copyright   :  (c) Amy de Buitléir 2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Utility functions that don't fit anywhere else.
--
------------------------------------------------------------------------
module Main where

import ALife.Realtra.Wain (run, finishRound)
import qualified ALife.Realtra.Config as Config
import ALife.Creatur.Daemon (Daemon(..), launch)
import ALife.Creatur.Universe (Universe, writeToLog)
import ALife.Creatur.Task (simpleDaemon, runInteractingAgents)
import Control.Monad.State (execStateT)
import Data.Version (showVersion)
import Paths_creatur_realtra (version)

startupHandler :: Universe u => String -> u -> IO u
startupHandler programName = execStateT (writeToLog $ "Starting " ++ programName)

shutdownHandler :: Universe u => String -> u -> IO ()
shutdownHandler programName u =
  execStateT (writeToLog $ "Shutdown requested for " ++ programName) u
  >> return ()

main :: IO ()
main = launch daemon Config.universe
  where daemon = simpleDaemon
                   {task=runInteractingAgents run finishRound,
                    onStartup=startupHandler programName,
                    onShutdown=shutdownHandler programName,
                    sleepTime=Config.sleepBetweenTasks}
        programName = "Réaltra v" ++ showVersion version
