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
import ALife.Creatur.Wain (conflation)
import ALife.Realtra.Wain (Astronomer)
import qualified ALife.Realtra.Config as Config
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)

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
  putStrLn $ show a
  putStrLn $ "Conflation=" ++ show (conflation a)

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
