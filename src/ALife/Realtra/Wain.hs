------------------------------------------------------------------------
-- |
-- Module      :  ALife.Realtra.Wain
-- Copyright   :  (c) Amy de Buitléir 2012-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A data mining agent, designed for the Créatúr framework.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module ALife.Realtra.Wain
  (
    Astronomer,
    run,
    randomAstronomer,
    summarise
  ) where

import ALife.Creatur (Agent, agentId)
import ALife.Creatur.Logger (writeToLog)
import ALife.Creatur.Universe (SimpleUniverse)
import ALife.Creatur.Wain (Wain(..), Label, adjustEnergy, adjustPassion,
  tryMating, numberOfClassifierModels, numberOfDeciderModels,
  conflation, chooseAction, randomWain, classify, teachLabel, hasChild,
  weanChildIfReady, incAge)
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.Statistics (Statistical, Statistic, stats,
  maxStats, minStats, avgStats, dStat)
import ALife.Realtra.Action (Action(..))
import qualified ALife.Realtra.Config as Config
import ALife.Realtra.Image (Image, stripedImage, randomImage)
import ALife.Realtra.ImageDB (anyImage)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (Rand, RandomGen ) --, evalRandIO, fromList)
import Control.Monad.State.Lazy (StateT, evalStateT)
import System.Random (randomIO, randomRIO)

type Astronomer = Wain Image Action

randomAstronomer :: RandomGen r => String -> Rand r Astronomer
randomAstronomer wainName = do
  let n = fromIntegral $ 3*Config.maxDeciderSize*Config.maxDeciderSize
  let app = stripedImage Config.imageWidth Config.imageHeight
  imgs <- replicateM n (randomImage Config.imageWidth Config.imageHeight)
  randomWain wainName app Config.maxClassifierSize imgs
    Config.maxDeciderSize Config.maxAgeOfMaturity

-- TODO Clean up this code
run
  :: [Astronomer]
    -> StateT (SimpleUniverse Astronomer) IO ([Astronomer], Maybe [Statistic])
run (me:xs) = do
  writeToLog $ "It's " ++ agentId me ++ "'s turn to shine"
  writeToLog $ "DEBUG: before " ++ show (stats me)
  let (me2, metabolismStats) = runMetabolism me
  writeToLog $ "DEBUG: after " ++ show (stats me2)
  (x, y) <- chooseObjects xs
  (imgLabel, action, me3)
    <- chooseAction (objectAppearance x) (objectAppearance y) me2
  writeToLog $ agentId me ++ " sees " ++ objectId x ++ ", labels it "
    ++ show imgLabel ++ ", and chooses to " ++ show action
    ++ " with " ++ objectId y
  (me4:others) <- runAction action me3 x y imgLabel
  me5 <- incAge me4
  us <- weanChildIfReady me5
  let modifiedAgents = us ++ others
  writeToLog $ "End of " ++ agentId me ++ "'s turn"
  writeToLog $ "Modified agents: " ++ show (map agentId modifiedAgents)
  return (modifiedAgents, Just (stats me ++ metabolismStats))
run _ = error "no more wains"

runMetabolism :: Astronomer -> (Astronomer, [Statistic])
runMetabolism me = (me', metabolismStats)
  where classifierDelta = Config.energyDeltaPerClassifierModel
                          * fromIntegral (numberOfClassifierModels me)
        classifierStat = dStat "classifier Δe" classifierDelta
        deciderDelta = Config.energyDeltaPerDeciderModel
                          * fromIntegral (numberOfDeciderModels me)
        deciderStat = dStat "decider Δe" deciderDelta
        conflationDelta = Config.conflationEnergyDeltaFactor
                            * (conflation me)
        conflationStat = dStat "conflation Δe" conflationDelta
        childDelta = if hasChild me
                      then Config.childRearingEnergyDelta
                      else 0
        childStat = dStat "child rearing Δe" childDelta
        passionStat = dStat "passion Δp" Config.passionDelta
        metabolismDelta = classifierDelta + deciderDelta
                            + conflationDelta + childDelta
        metabolismStats = [classifierStat, deciderStat, conflationStat,
          childStat, passionStat]
        me' = adjustPassion Config.passionDelta . adjustEnergy metabolismDelta $ me

chooseObjects :: [Astronomer] -> StateT (SimpleUniverse Astronomer) IO (Object, Object)
chooseObjects xs = do
  -- writeToLog $ "Direct object = " ++ objectId x
  -- writeToLog $ "Indirect object = " ++ objectId y
  (x:y:_) <- liftIO . randomlyInsertImages . map AObject $ xs
  return (x, y)

data Object = IObject Image String | AObject Astronomer

objectId :: Object -> String
objectId (IObject _ s) = "Image " ++ s
objectId (AObject a) = agentId a

objectAppearance :: Object -> Image
objectAppearance (IObject img _) = img
objectAppearance (AObject a) = appearance a

randomlyInsertImages :: [Object] -> IO [Object]
randomlyInsertImages xs = do
  insert <- randomIO
  if insert
    then do
      (img, imageId) <- evalStateT anyImage Config.imageDB
      n <- randomRIO (0, 1)
      let (fore, aft) = splitAt n xs
      randomlyInsertImages $ fore ++ IObject img imageId : aft
    else
      return xs

runAction :: Action -> Astronomer -> Object -> Object -> Label
  -> StateT (SimpleUniverse Astronomer) IO [Astronomer]

--
-- Co-operate
--
runAction Cooperate me (IObject img imgId) (AObject other) myLabel = do
  writeToLog $ agentId me ++ " tells " ++ agentId other
    ++ " that image " ++ imgId ++ " has label "
    ++ show myLabel
  let (otherLabel, other') = classify img other
  if myLabel == otherLabel
    then do
       writeToLog $ agentId other ++ " agrees with " ++  agentId me
         ++ " that " ++ imgId ++ " has label "
         ++ show myLabel
       return [adjustEnergy (Config.cooperationEnergyDelta
                 + Config.cooperationAgreementDelta) me, other']
    else do
       writeToLog $ agentId other ++ " disagrees with " ++  agentId me
         ++ ", says that " ++ imgId ++ " has label "
         ++ show otherLabel
       writeToLog $ agentId me ++ " is learning"
       writeToLog $ agentId other' ++ " is learning"
       me' <- teachLabel img otherLabel
               . adjustEnergy Config.cooperationEnergyDelta $ me
       other'' <- teachLabel img myLabel other'
       return [me', other'']
runAction Cooperate me _ _ _ = do
  writeToLog $ agentId me ++ " tries to co-operate with an image"
  return [adjustEnergy Config.cooperationEnergyDelta me]
  
--
-- Mate
--
runAction Mate me (AObject other) _ _ = do
  writeToLog $ agentId me ++ " looks for a mate"
  tryMating me other Config.flirtingEnergyDelta Config.matingEnergyDelta

runAction Mate me (IObject _ imgId) _ _ = do
  writeToLog $ agentId me ++ " flirted with image " ++ imgId
  return [adjustEnergy Config.flirtingEnergyDelta me]

--
-- Ignore
--
runAction Ignore me (AObject other) _ _ = do
  writeToLog $ agentId me ++ " ignores " ++ agentId other
  return [me]
runAction Ignore me (IObject _ imgId) _ _ = do
  writeToLog $ agentId me ++ " ignores image " ++ imgId
  return [me]

summarise :: [[Statistic]] -> StateT (SimpleUniverse Astronomer) IO ()
summarise xs = do
  writeToLog $ pretty (maxStats xs)
  writeToLog $ pretty (minStats xs)
  writeToLog $ pretty (avgStats xs)
