------------------------------------------------------------------------
-- |
-- Module      :  ALife.Realtra.Classification
-- Copyright   :  (c) Amy de Buitléir 2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A set of counts of the number of times each classification
-- category has been used, which persists between runs.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module ALife.Realtra.Classification
  (
    Counters,
    PersistentCounters,
    mkPersistentCounters,
    incrementCount,
    -- lookupCount,
    rarityOf
  ) where

import ALife.Creatur.Persistent (Persistent, mkPersistent, getPS, putPS)
import ALife.Creatur.Wain (Label)
import Control.Monad.State (StateT)
import qualified Data.Map as M

type Counters = M.Map Label Int

type PersistentCounters = Persistent Counters

mkPersistentCounters :: FilePath -> PersistentCounters
mkPersistentCounters = mkPersistent M.empty

incrementCount :: Label -> StateT PersistentCounters IO ()
incrementCount key = do
  m <- getPS
  putPS (M.insertWith (+) key 1 m)

-- lookupCount :: Label -> StateT PersistentCounters IO Int
-- lookupCount key = do
--   m <- getPS
--   return (M.findWithDefault 0 key m)

rarityOf :: Label -> StateT PersistentCounters IO Double
rarityOf key = do
  m <- getPS
  let cTotal = M.foldl' (+) 0 m
  let c = M.findWithDefault 0 key m
  if cTotal == 0
    then return 1
    else return $ 1 - (fromIntegral c)/(fromIntegral cTotal)
