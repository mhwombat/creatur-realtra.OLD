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

import ALife.Creatur.Universe
import ALife.Creatur.Wain
import ALife.Creatur.Wain.Brain
import ALife.Creatur.Wain.Condition
import ALife.Creatur.Wain.Response
import qualified ALife.Creatur.Wain.Scenario as Scenario
import ALife.Creatur.Wain.GeneticSOM
import qualified ALife.Realtra.Config as Config
import ALife.Realtra.Action
import ALife.Realtra.Wain
import Control.Monad.State
import Data.List
import Math.Geometry.GridMap (elems)
import System.Environment
import Text.Printf (printf)

getAndExamineAll
  :: (Universe u, Agent u ~ Astronomer)
    => StateT u IO ()
getAndExamineAll = do
  names <- agentIds
  mapM_ getAndExamine names
  
getAndExamine
  :: (Universe u, Agent u ~ Astronomer)
    => String -> StateT u IO ()
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
  putStrLn $ "devotion: " ++ show (devotion a)
  putStrLn $ "ageOfMaturity: " ++ show (ageOfMaturity a)
  putStrLn $ "passionDelta: " ++ show (passionDelta a)
  putStrLn $ "energy: " ++ printf "%5.3f" (energy a)
  putStrLn $ "passion: " ++ printf "%5.3f" (passion a)
  putStrLn $ "age: " ++ show (age a)
  putStrLn $ "total # children borne: "
    ++ show (childrenBorneLifetime a)
  putStrLn $ "total # children weaned: "
    ++ show (childrenWeanedLifetime a)
  putStrLn $ "litter size: " ++ show (length $ litter a)
  putStrLn $ "counts=" ++ show (elems . counterMap . classifier $ brain a)
  putStrLn $ "size: " ++ show (wainSize a)
  putStrLn $ "Classifier size: " ++ show (size . classifier . brain $ a)
  putStrLn $ "Number of classifier models: " ++ show (numModels . classifier . brain $ a)
  putStrLn $ "Classifier learning function " ++ show (learningFunction . classifier . brain $ a)
  putStrLn $ "Decider size: " ++ show (size . decider . brain $ a)
  putStrLn $ "Number of decider models: " ++ show (numModels . decider . brain $ a)
  putStrLn $ "Decider learning function " ++ show (learningFunction . decider . brain $ a)
  -- putStrLn "------------------------"
  -- putStrLn "Mental models of vectors"
  -- putStrLn "------------------------"
  -- mapM_ putStrLn $ concatMap (prettyAudioPattern 9) (toList . classifier . brain $ a)
  putStrLn "-----------------"
  putStrLn "Response models"
  putStrLn "-----------------"
  mapM_ putStrLn $ concatMap prettyResponseModel (toList . decider . brain $ a)
  -- putStrLn "--------"
  -- putStrLn "Raw data"
  -- putStrLn "--------"
  -- putStrLn $ show a

prettyResponseModel :: (Label, Response Action) -> [String]
prettyResponseModel (l, r) = do
  [ "Model " ++ show l,
    "Differences: "
      ++ formatVector "%5.3f" (Scenario.directObject . scenario $ r),
    "Energy: " ++ show (cEnergy . Scenario.condition . scenario $ r),
    "Passion: " ++ show (cPassion . Scenario.condition . scenario $ r),
    "Action: " ++ show (action r),
    "Expected happiness change: "
      ++ maybe "" (printf "%.3g") (outcome r),
    "-----" ]

formatVector :: String -> [Double] -> String
formatVector fmt = intercalate " " . map (printf fmt)

main :: IO ()
main = do
  t <- evalStateT currentTime (universe Config.config)
  putStrLn $ "Universe time is " ++ show t

  args <- getArgs
  if null args
    then
      evalStateT getAndExamineAll (universe Config.config)
    else do
      let s = head args
      evalStateT (getAndExamine s) (universe Config.config)
