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

import ALife.Creatur.Universe (SimpleUniverse, agentIds, getAgent,
  currentTime)
import ALife.Creatur.Wain
import ALife.Realtra.Wain (Astronomer, schemaQuality,
  categoriesReallyUsed)
import ALife.Creatur.Wain.Brain (classifier)
import ALife.Creatur.Wain.GeneticSOM (counterMap)
import qualified ALife.Realtra.Config as Config
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.IO.Class (liftIO)
import Math.Geometry.GridMap (elems)
import System.Environment (getArgs)
import Text.Printf (printf)

getAndExamineAll
  :: StateT (SimpleUniverse Astronomer) IO ()
getAndExamineAll = do
  names <- agentIds
  mapM_ getAndExamine names
  
getAndExamine
  :: String -> StateT (SimpleUniverse Astronomer) IO ()
getAndExamine s = do
  a <- getAgent s
  case a of
    (Right agent) -> liftIO $ examine agent
    (Left msg)    -> liftIO $ putStrLn msg 
  
examine :: Astronomer -> IO ()
examine a = do
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
  putStrLn $ "schema quality=" ++  printf "%5.3f" (schemaQuality a)
  putStrLn $ "categories really used=" ++ show (categoriesReallyUsed a)
  putStrLn $ "size: " ++ show (size a)

main :: IO ()
main = do
  t <- evalStateT currentTime
        (Config.universe Config.config :: SimpleUniverse Astronomer)
  putStrLn $ "Universe time is " ++ show t

  args <- getArgs
  if null args
    then
      evalStateT (getAndExamineAll) (Config.universe Config.config)
    else do
      let s = head args
      evalStateT (getAndExamine s) (Config.universe Config.config)
