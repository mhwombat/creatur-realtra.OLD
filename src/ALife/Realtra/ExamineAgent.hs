------------------------------------------------------------------------
-- |
-- Module      :  ALife.Realtra.ExamineAgent
-- Copyright   :  (c) Amy de Buitléir 2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
module Main where

import ALife.Creatur.Database (size)
import ALife.Creatur.Universe (CachedUniverse, agentIds, getAgent,
  currentTime)
import ALife.Creatur.Wain
import ALife.Realtra.Wain (Astronomer, Config, schemaQuality,
  categoriesReallyUsed, universe, maxCategories)
import ALife.Creatur.Wain.Brain (classifier)
import ALife.Creatur.Wain.GeneticSOM (counterMap)
import qualified ALife.Realtra.Config as Config
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.IO.Class (liftIO)
import Math.Geometry.GridMap (elems)
import System.Environment (getArgs)
import Text.Printf (printf)

getAndExamineAll
  :: Config -> StateT (CachedUniverse Astronomer) IO ()
getAndExamineAll config = do
  names <- agentIds
  mapM_ (getAndExamine config) names
  
getAndExamine
  :: Config -> String -> StateT (CachedUniverse Astronomer) IO ()
getAndExamine config s = do
  a <- getAgent s
  case a of
    (Right agent) -> liftIO $ examine config agent
    (Left msg)    -> liftIO $ putStrLn msg 
  
examine :: Config -> Astronomer -> IO ()
examine config a = do
  putStrLn $ "name: " ++ show (name a)
  -- appearance
  -- brain
  putStrLn $ "ageOfMaturity: " ++ show (ageOfMaturity a)
  putStrLn $ "passionDelta: " ++ show (passionDelta a)
  putStrLn $ "energy: " ++ printf "%5.3f" (energy a)
  putStrLn $ "passion: " ++ printf "%5.3f" (passion a)
  putStrLn $ "age: " ++ show (age a)
  putStrLn $ "numberOfChildren: " ++ show (numberOfChildren a)
  putStrLn $ "litter size: " ++ show (length $ litter a)
  putStrLn $ "counts=" ++ show (elems . counterMap . classifier $ brain a)
  let mc = maxCategories config
  putStrLn $ "schema quality=" ++  printf "%5.3f" (schemaQuality mc a)
  putStrLn $ "categories really used=" ++ show (categoriesReallyUsed mc a)
  putStrLn $ "size: " ++ show (size a)

main :: IO ()
main = do
  t <- evalStateT currentTime (universe Config.config)
  putStrLn $ "Universe time is " ++ show t

  args <- getArgs
  if null args
    then
      evalStateT (getAndExamineAll Config.config) (universe Config.config)
    else do
      let s = head args
      evalStateT (getAndExamine Config.config s) (universe Config.config)
