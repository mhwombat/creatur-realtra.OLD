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

import ALife.Creatur.Database as D (Database, DBRecord)
import ALife.Creatur.Universe (Universe, agentIds, getAgent)
import ALife.Creatur.Clock (currentTime)
import ALife.Realtra.Wain (Astronomer)
import qualified ALife.Realtra.Config as Config
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)

getAndExamineAll
  :: (Database d, DBRecord d ~ Astronomer)
    => StateT (Universe c l d n x Astronomer) IO ()
getAndExamineAll = do
  names <- agentIds
  mapM_ getAndExamine names
  
getAndExamine
  :: (Database d, DBRecord d ~ Astronomer)
    => String -> StateT (Universe c l d n x Astronomer) IO ()
getAndExamine s = do
  a <- getAgent s
  case a of
    (Right agent) -> liftIO $ examine agent
    (Left msg)    -> liftIO $ putStrLn msg 
  
examine :: Astronomer -> IO ()
examine a = do
  putStrLn $ show a

main :: IO ()
main = do
  t <- evalStateT currentTime Config.universe
  putStrLn $ "Universe time is " ++ show t

  args <- getArgs
  if null args
    then
      evalStateT (getAndExamineAll) Config.universe
    else do
      let s = head args
      evalStateT (getAndExamine s) Config.universe
