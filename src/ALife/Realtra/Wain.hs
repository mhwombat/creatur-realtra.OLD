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
    randomAstronomer
  ) where

import ALife.Creatur (Agent, agentId)
import ALife.Creatur.Logger (writeToLog)
import ALife.Creatur.Universe (SimpleUniverse)
import ALife.Creatur.Wain (Wain(..), Label, adjustEnergy, tryMating,
  numberOfClassifierModels, numberOfDeciderModels, chooseAction,
  randomWain, classify, teachLabel, hasChild, weanChildIfReady)
import ALife.Creatur.Wain.Statistics (Statistical, stats) --, iStat)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
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
  randomWain wainName app Config.maxClassifierSize imgs Config.maxDeciderSize Config.maxAgeOfMaturity

run :: [Astronomer] -> StateT (SimpleUniverse Astronomer) IO [Astronomer]
run (me:xs) = do
  writeToLog $ "It's " ++ agentId me ++ "'s turn to shine"
  writeToLog $ "DEBUG: before " ++ show (stats me)
  let bc = metabolismCost me
  writeToLog $ "DEBUG: metabolism cost=" ++ show bc
  let me' = adjustEnergy bc me
  writeToLog $ "DEBUG: after " ++ show (stats me')
  (x:y:_) <- liftIO . randomlyInsertImages . map AObject $ xs
  writeToLog $ "DEBUG: direct object = " ++ objectId x
  writeToLog $ "DEBUG: indirect object = " ++ objectId y
  let (imgLabel, action, me3) = chooseAction (objectAppearance x) (objectAppearance y) me'
  writeToLog $ agentId me ++ " sees " ++ objectId x ++ ", labels it " ++ show imgLabel ++ ", and choses to " ++ show action
  (me4:others) <- runAction action me3 x y imgLabel
  me5 <- weanChildIfReady me4
  return $ me5 ++ others
run _ = error "no more wains"

metabolismCost :: Astronomer -> UIDouble
metabolismCost me = classifierCost + deciderCost + childCost
  where classifierCost = Config.energyDeltaPerClassifierModel
                          * fromIntegral (numberOfClassifierModels me)
        deciderCost = Config.energyDeltaPerDeciderModel
                          * fromIntegral (numberOfDeciderModels me)
        childCost = if hasChild me
                      then Config.childRearingEnergyDelta
                      else 0

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
      randomlyInsertImages $ fore ++ (IObject img imageId) : aft
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
       return [teachLabel img otherLabel
               . adjustEnergy Config.cooperationEnergyDelta $ me,
               teachLabel img myLabel other']
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
  return []
runAction Ignore me (IObject _ imgId) _ _ = do
  writeToLog $ agentId me ++ " ignores image " ++ imgId
  return []
