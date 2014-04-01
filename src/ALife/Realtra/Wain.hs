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
{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}
module ALife.Realtra.Wain
  (
    Astronomer,
    run,
    randomAstronomer,
    finishRound,
    summarise,
    energy,
    passion,
    schemaQuality,
    categoriesReallyUsed
  ) where

import ALife.Creatur (Agent, agentId)
import ALife.Creatur.Universe (Universe, writeToLog, popSize)
import ALife.Creatur.Wain (Wain(..), Label, adjustEnergy, adjustPassion,
  chooseAction, randomWain, classify, teachLabel, incAge,
  weanMatureChildren, tryMating, energy, passion)
import ALife.Creatur.Wain.Brain (classifier)
import ALife.Creatur.Wain.GeneticSOM (counterMap)
import ALife.Creatur.Wain.Pretty (pretty)
import qualified ALife.Creatur.Wain.Statistics as Stats
import ALife.Realtra.Action (Action(..))
import qualified ALife.Realtra.Config as C
import ALife.Creatur.Wain.PersistentStatistics (updateStats, readStats,
  clearStats, summarise)
import ALife.Realtra.Image (Image, stripedImage, randomImage)
import ALife.Realtra.ImageDB (anyImage)
import Control.Monad (replicateM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (Rand, RandomGen)
import Control.Monad.State.Lazy (StateT, evalStateT)
import Data.Word (Word8)
import Factory.Math.Statistics (getStandardDeviation)
import Math.Geometry.GridMap (elems)
import System.Random (randomIO, randomRIO)

type Astronomer = Wain Image Action

randomAstronomer
  :: RandomGen r => String -> Word8 -> Word8 -> Rand r Astronomer
randomAstronomer wainName classifierSize deciderSize = do
  let n = fromIntegral $ 3*classifierSize*classifierSize
  let w = C.imageWidth C.config
  let h = C.imageHeight C.config
  let app = stripedImage w h
  imgs <- replicateM n (randomImage w h)
  randomWain wainName app classifierSize imgs deciderSize
    (C.initialPopulationMaxAgeOfMaturity C.config)

data Result = Result
  {
    sizeEnergyDelta :: Double,
    crowdingEnergyDelta :: Double,
    classificationEnergyDelta :: Double,
    childRearingEnergyDelta :: Double,
    coopEnergyDelta :: Double,
    agreementEnergyDelta :: Double,
    flirtingEnergyDelta :: Double,
    matingEnergyDelta :: Double,
    netEnergyDelta :: Double,
    birthCount :: Int,
    weanCount :: Int,
    cooperateCount :: Int,
    agreeCount :: Int,
    flirtCount :: Int,
    mateCount :: Int,
    ignoreCount :: Int
  }

initResult :: Result
initResult = Result
  {
    sizeEnergyDelta = 0,
    crowdingEnergyDelta = 0,
    classificationEnergyDelta = 0,
    childRearingEnergyDelta = 0,
    coopEnergyDelta = 0,
    agreementEnergyDelta = 0,
    flirtingEnergyDelta = 0,
    matingEnergyDelta = 0,
    netEnergyDelta = 0,
    birthCount = 0,
    weanCount = 0,
    cooperateCount = 0,
    agreeCount = 0,
    flirtCount = 0,
    mateCount = 0,
    ignoreCount = 0
  }

resultStats :: Result -> [Stats.Statistic]
resultStats r =
  [
    Stats.uiStat "size Δe" (sizeEnergyDelta r),
    Stats.uiStat "pop Δe" (crowdingEnergyDelta r),
    Stats.uiStat "cat Δe" (classificationEnergyDelta r),
    Stats.uiStat "child rearing Δe" (childRearingEnergyDelta r),
    Stats.uiStat "cooperation Δe" (coopEnergyDelta r),
    Stats.uiStat "agreement Δe" (agreementEnergyDelta r),
    Stats.uiStat "flirting Δe" (flirtingEnergyDelta r),
    Stats.uiStat "mating Δe" (matingEnergyDelta r),
    Stats.uiStat "net Δe" (netEnergyDelta r),
    Stats.iStat "bore" (birthCount r),
    Stats.iStat "weaned" (weanCount r),
    Stats.iStat "co-operated" (cooperateCount r),
    Stats.iStat "agreed" (agreeCount r),
    Stats.iStat "flirted" (flirtCount r),
    Stats.iStat "mated" (mateCount r),
    Stats.iStat "ignored" (ignoreCount r)
  ]

runMetabolism ::  (Astronomer, Result) -> Int -> (Astronomer, Result)
runMetabolism (a, r) n = (a', r')
  where a' = adjustPassion $ adjustEnergy deltaE a
        r' = r {
                 sizeEnergyDelta = sc,
                 crowdingEnergyDelta = cc,
                 childRearingEnergyDelta = crc
               }
        deltaE = sc + cc + crc
        sc = sizeCost a
        cc = crowdingCost n
        crc = childRearingCost a

sizeCost :: Astronomer -> Double
sizeCost a = - (s/m)*(s/m)
  where s = fromIntegral (size a)
        m = fromIntegral (C.maxSize C.config)

crowdingCost :: Int -> Double
crowdingCost n = - (n'/m)*(n'/m)
  where n' = fromIntegral n
        m = fromIntegral (C.maxPopulation C.config)

schemaQuality :: Astronomer -> Double
schemaQuality = schemaQuality' . elems . counterMap . classifier . brain

schemaQuality' :: Integral a => [a] -> Double
schemaQuality' xs = n/xMax
  where n = min xMax . fromIntegral $ categoriesReallyUsed' xs
        xMax = fromIntegral $ C.maxCategories C.config
-- schemaQuality' xs = y*y
--   where y = 2*(x - 0.5)
--         x = (n - xMin)/(xMax - xMin)
--         n = min xMax . fromIntegral $ categoriesReallyUsed' xs
--         xMin = fromIntegral $ C.minCategories C.config
--         xMax = fromIntegral $ C.maxCategories C.config

categoriesReallyUsed :: Astronomer -> Int
categoriesReallyUsed
  = categoriesReallyUsed' . elems . counterMap . classifier . brain

categoriesReallyUsed' :: Integral a => [a] -> Int
categoriesReallyUsed' xs = length $ filter (>s) xs'
  where s = getStandardDeviation xs'
        xs' = map fromIntegral xs :: [Double]
-- categoriesReallyUsed' xs = length $ filter (>k) xs
--   where k = (sum xs) `div` (fromIntegral $ C.maxCategories C.config)

childRearingCost :: Astronomer -> Double
childRearingCost a = x * (sum . map f $ litter a)
    where x = C.childCostFactor C.config
          f c = sizeCost c
          
run
  :: Universe u
    => [Astronomer] -> StateT u IO [Astronomer]
run (me:xs) = do
  when (null xs) $ writeToLog "WARNING: Last wain standing!"
  writeToLog $ "---------- " ++ agentId me ++ "'s turn ----------"
  writeToLog $ "Next in line: " ++ show (map agentId $ take 3 xs)
  writeToLog $ "At beginning of turn, " ++ agentId me ++ "'s stats: "
    ++ pretty (Stats.stats me)
  k <- popSize
  writeToLog $ "DEBUG pop size=" ++ show k
  let (me2, r) = runMetabolism (me, initResult) k
  writeToLog $ "DEBUG metabolism finished"
  (x, y) <- chooseObjects xs
  writeToLog $ agentId me ++ " sees " ++ objectId x
    ++ " and " ++ objectId y
  (imgLabel, action, me3)
    <- chooseAction (objectAppearance x) (objectAppearance y) me2
  writeToLog $ agentId me ++ " sees " ++ objectId x ++ ", labels it "
    ++ show imgLabel ++ ", and chooses to " ++ show action
    ++ " with " ++ objectId y
  (me4:others, r4) <- runAction action (me3, r) x y imgLabel
  me5 <- incAge me4
  (me6:weanlings, r6) <- wean (me5, r4)
  let r7 = r6 { netEnergyDelta = energy me6 - energy me}
  let stats = Stats.stats me6 ++ resultStats r7
  writeToLog $ "End of " ++ agentId me ++ "'s turn"
  writeToLog $ "At end of turn, " ++ agentId me ++ "'s stats: "
    ++ pretty stats
  let modifiedAgents = me6:weanlings ++ others
  writeToLog $ "Modified agents: " ++ show (map agentId modifiedAgents)
  updateStats stats (C.statsFile C.config)
  return modifiedAgents
run _ = error "no more wains"

chooseObjects
  :: Universe u
    => [Astronomer] -> StateT u IO (Object, Object)
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
      (img, imageId) <- evalStateT anyImage (C.imageDB C.config)
      n <- randomRIO (0, 1)
      let (fore, aft) = splitAt n xs
      randomlyInsertImages $ fore ++ IObject img imageId : aft
    else
      return xs

runAction
  :: Universe u
    => Action -> (Astronomer, Result) -> Object -> Object -> Label
      -> StateT u IO ([Astronomer], Result)

--
-- Co-operate
--
runAction Cooperate (a, r) dObj (AObject b) aLabel = do
  let dObjId = objectId dObj
  let dObjApp = objectAppearance dObj
  writeToLog $ agentId a ++ " tells " ++ agentId b
    ++ " that image " ++ dObjId ++ " has label "
    ++ show aLabel
  let (bLabel, b') = classify dObjApp b
  if aLabel == bLabel
    then agree ([a,b'], r) dObj aLabel
    else disagree ([a,b'], r) dObj aLabel bLabel
runAction Cooperate (a, r) _ _ _ = do
  writeToLog $ agentId a ++ " tries to co-operate with an image"
  return $ rewardCooperation ([a], r)
  
--
-- Flirt
--
runAction Flirt (a, r) (AObject b) _ _ = do
  writeToLog $ agentId a ++ " looks for a mate"
  flirt ([a,b], r)

runAction Flirt (a, r) (IObject _ imgId) _ _ = do
  writeToLog $ agentId a ++ " flirted with image " ++ imgId
  return $ rewardFlirtation ([a], r)

--
-- Ignore
--
runAction Ignore (a, r) obj _ _ = do
  writeToLog $ agentId a ++ " ignores " ++ objectId obj
  return ([a], r { ignoreCount = ignoreCount r + 1 })

--
-- Utility functions
--
agree
  :: Universe u
    => ([Astronomer], Result) -> Object -> Label
      -> StateT u IO ([Astronomer], Result)
agree x@((a:b:_), _) dObj label = do
  let dObjId = objectId dObj
  writeToLog $ agentId b ++ " agrees with " ++  agentId a
    ++ " that " ++ dObjId ++ " has label " ++ show label
  return . rewardCooperation $ rewardAgreement x
agree _ _ _ = error "Passed too few agents to agree"

disagree
  :: Universe u
    => ([Astronomer], Result) -> Object -> Label -> Label
      -> StateT u IO ([Astronomer], Result)
disagree ((a:b:cs), r) dObj aLabel bLabel = do
  let dObjId = objectId dObj
  let dObjApp = objectAppearance dObj
  writeToLog $ agentId b ++ " disagrees with " ++  agentId a
    ++ ", says that " ++ dObjId ++ " has label "
    ++ show bLabel
  a' <- teachLabel dObjApp bLabel
          . adjustEnergy (C.cooperationEnergyDelta C.config) $ a
  b' <- teachLabel dObjApp aLabel b
  return $ rewardCooperation (a':b':cs,r)
disagree _ _ _ _ = error "Passed too few agents to disagree"

rewardCooperation :: ([Astronomer], Result) -> ([Astronomer], Result)
rewardCooperation (a:bs, r) = (a':bs, r')
  where a' = adjustEnergy deltaE a
        r' = r { coopEnergyDelta = deltaE,
                 cooperateCount = cooperateCount r + 1}
        deltaE = C.cooperationEnergyDelta C.config
rewardCooperation _ = error "Passed too few agents to rewardCooperation"

rewardAgreement :: ([Astronomer], Result) -> ([Astronomer], Result)
rewardAgreement (a:b:cs, r) = (a':b':cs, r')
  where a' = adjustEnergy deltaE a
        b' = adjustEnergy deltaE b
        r' = r { agreementEnergyDelta = deltaE,
                 agreeCount = agreeCount r + 1}
        deltaE = (schemaQuality a)
                   *(C.cooperationAgreementDelta C.config)
rewardAgreement _ = error "Passed too few agents to rewardAgreement"

flirt
  :: Universe u
    => ([Astronomer], Result) -> StateT u IO ([Astronomer], Result)
flirt (ws, r) = do
  let (a:b:others, r2) = rewardFlirtation (ws, r)
  (ws', mated) <- tryMating a b
  if mated
    then return . recordBirths $ rewardMating (ws', r2)
    else return (ws' ++ others, r2)

recordBirths :: ([Astronomer], Result) -> ([Astronomer], Result)
recordBirths (ws, r) = (ws, r')
  where r' = r { birthCount=length . litter $ head ws }

rewardFlirtation :: ([Astronomer], Result) -> ([Astronomer], Result)
rewardFlirtation (a:others, r) = (a':others, r')
  where a' = adjustEnergy deltaE a
        r' = r { flirtingEnergyDelta=deltaE,
                 flirtCount=flirtCount r + 1}
        deltaE = C.flirtingEnergyDelta C.config
rewardFlirtation x = x

rewardMating :: ([Astronomer], Result) -> ([Astronomer], Result)
rewardMating (a:others, r) = (a':others, r')
  where a' = adjustEnergy deltaE a
        r' = r { matingEnergyDelta=deltaE,
                 mateCount=mateCount r + 1}
        deltaE = C.flirtingEnergyDelta C.config
rewardMating x = x

wean
  :: Universe u
    => (Astronomer, Result) -> StateT u IO ([Astronomer], Result)
wean (a, r) = do
  as <- weanMatureChildren a
  let r' = r { weanCount = weanCount r + length as - 1 }
  return (as, r')

finishRound :: Universe u => StateT u IO ()
finishRound = do
  xs <- readStats $ C.statsFile C.config
  summarise xs
  clearStats $ C.statsFile C.config
