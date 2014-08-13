------------------------------------------------------------------------
-- |
-- Module      :  ALife.Realtra.Universe
-- Copyright   :  (c) Amy de Buitléir 2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides a habitat for astronomer wains.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module ALife.Realtra.Universe
  (
    RUniverse,
    mkRUniverse,
    incrementCount,
    rarityOf
  ) where

import qualified ALife.Creatur.Universe as U
import qualified ALife.Creatur as A
import qualified ALife.Creatur.Namer as N
import qualified ALife.Creatur.Checklist as CL
import qualified ALife.Creatur.Counter as K
import qualified ALife.Creatur.Database as D
import qualified ALife.Creatur.Database.CachedFileSystem as CFS
import qualified ALife.Creatur.Logger.SimpleLogger as SL
import qualified ALife.Creatur.EnergyPool as E
import ALife.Creatur.Wain (Label)
import ALife.Creatur.Util (stateMap)
import qualified ALife.Realtra.Classification as Ctr
import Control.Monad.State (StateT, get)

data RUniverse a = RUniverse
  {
    cuClock :: K.PersistentCounter,
    cuLogger :: SL.SimpleLogger,
    cuDB :: CFS.CachedFSDatabase a,
    cuNamer :: N.SimpleNamer,
    cuChecklist :: CL.PersistentChecklist,
    cuEnergyPool :: E.PersistentEnergyPool,
    cuCounters :: Ctr.PersistentCounters
  } deriving (Show, Eq)

instance (A.Agent a, D.SizedRecord a) => U.Universe (RUniverse a) where
  type Agent (RUniverse a) = a
  type Clock (RUniverse a) = K.PersistentCounter
  clock = cuClock
  setClock u c = u { cuClock=c }
  type Logger (RUniverse a) = SL.SimpleLogger
  logger = cuLogger
  setLogger u l = u { cuLogger=l }
  type AgentDB (RUniverse a) = CFS.CachedFSDatabase a
  agentDB = cuDB
  setAgentDB u d = u { cuDB=d }
  type Namer (RUniverse a) = N.SimpleNamer
  agentNamer = cuNamer
  setNamer u n = u { cuNamer=n }
  type Checklist (RUniverse a) = CL.PersistentChecklist
  checklist = cuChecklist
  setChecklist u cl = u { cuChecklist=cl }
  type EnergyPool (RUniverse a) = E.PersistentEnergyPool
  energyPool = cuEnergyPool
  setEnergyPool u cl = u { cuEnergyPool=cl }

mkRUniverse :: String -> FilePath -> Int -> RUniverse a
mkRUniverse name dir cacheSize
  = RUniverse c l d n cl e k
  where c = K.mkPersistentCounter (dir ++ "/clock")
        l = SL.mkSimpleLogger (dir ++ "/log/" ++ name ++ ".log")
        d = CFS.mkCachedFSDatabase (dir ++ "/db") cacheSize
        n = N.mkSimpleNamer (name ++ "_") (dir ++ "/namer")
        cl = CL.mkPersistentChecklist (dir ++ "/todo")
        e = E.mkPersistentEnergyPool (dir ++ "/energy")
        k = Ctr.mkPersistentCounters (dir ++ "/counters")

setCounters :: RUniverse a -> Ctr.PersistentCounters -> RUniverse a
setCounters u c = u { cuCounters = c }

-- withCounters
--   :: (Universe u, Monad m)
--     => StateT (Ctr.Counters u) m a -> StateT u m a
withCounters
  :: Monad m
    => StateT Ctr.PersistentCounters m b -> StateT (RUniverse a) m b
withCounters program = do
  u <- get
  stateMap (setCounters u) cuCounters program

incrementCount :: Label -> StateT (RUniverse a) IO ()
incrementCount key = withCounters $ Ctr.incrementCount key

rarityOf :: Label -> StateT (RUniverse a) IO Double
rarityOf key = withCounters $ Ctr.rarityOf key
