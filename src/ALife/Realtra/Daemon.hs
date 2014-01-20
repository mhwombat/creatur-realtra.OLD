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

main :: IO ()
main = launch simpleDaemon{task=runInteractingAgents run summarise}
         Config.universe
