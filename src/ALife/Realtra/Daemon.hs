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

import ALife.Realtra.Wain (run, summarise)
import qualified ALife.Realtra.Config as Config
import ALife.Creatur.Daemon (Daemon(..), launch)
import ALife.Creatur.Universe.Task (simpleDaemon, runInteractingAgents)
import Paths_creatur_realtra (version)
import Data.Version (showVersion)

main :: IO ()
main = launch daemon Config.universe
  where daemon = (simpleDaemon programName)
                   {task=runInteractingAgents run summarise}
        programName = "Réaltra v" ++ showVersion version

