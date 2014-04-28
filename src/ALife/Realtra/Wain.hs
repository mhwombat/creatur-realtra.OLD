------------------------------------------------------------------------
-- |
-- Module      :  ALife.Realtra.Wain
-- Copyright   :  (c) Amy de Buitléir 2012-2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A data mining agent, designed for the Créatúr framework.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables,
    TemplateHaskell, Rank2Types #-}
module ALife.Realtra.Wain
  (
    Config(..),
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

import ALife.Creatur (agentId)
import ALife.Creatur.Database (size)
import ALife.Creatur.Universe (CachedUniverse, writeToLog, popSize)
import ALife.Creatur.Util (stateMap)
import ALife.Creatur.Wain (Wain(..), Label, adjustEnergy, adjustPassion,
  chooseAction, randomWain, classify, teachLabel, incAge,
  weanMatureChildren, tryMating, energy, passion, hasLitter)
import ALife.Creatur.Wain.Brain (classifier)
import ALife.Creatur.Wain.GeneticSOM (counterMap)
import ALife.Creatur.Wain.Pretty (pretty)
import qualified ALife.Creatur.Wain.Statistics as Stats
import ALife.Realtra.Action (Action(..))
import ALife.Creatur.Wain.PersistentStatistics (updateStats, readStats,
  clearStats, summarise)
import ALife.Realtra.Image (Image, stripedImage, randomImage)
import ALife.Realtra.ImageDB (ImageDB, anyImage)
import Control.Lens hiding (Action, universe)
import Control.Monad (replicateM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (Rand, RandomGen)
import Control.Monad.State.Lazy (StateT, execStateT, evalStateT, get)
import Data.Word (Word8, Word16)
-- import Factory.Math.Statistics (getStandardDeviation)
import Math.Geometry.GridMap (elems)
import System.Random (randomIO, randomRIO)
import Text.Printf (printf)

data Object = IObject Image String | AObject Astronomer

isImage :: Object -> Bool
isImage (IObject _ _) = True
isImage (AObject _) = False

objectId :: Object -> String
objectId (IObject _ s) = "Image " ++ s
objectId (AObject a) = agentId a

objectAppearance :: Object -> Image
objectAppearance (IObject img _) = img
objectAppearance (AObject a) = appearance a

addIfAgent :: Object -> [Astronomer] -> [Astronomer]
addIfAgent (IObject _ _) xs = xs
addIfAgent (AObject a) xs = a:xs

randomlyInsertImages :: ImageDB -> [Object] -> IO [Object]
randomlyInsertImages db xs = do
  insert <- randomIO
  if insert
    then do
      (img, imageId) <- evalStateT anyImage db
      n <- randomRIO (0, 1)
      let (fore, aft) = splitAt n xs
      randomlyInsertImages db $ fore ++ IObject img imageId : aft
    else
      return xs

type Astronomer = Wain Image Action

-- TODO: Redo with lenses

randomAstronomer
  :: RandomGen r
    => String -> Int -> Int -> Word8 -> Word8 -> Word16 -> Rand r Astronomer
randomAstronomer wainName w h classifierSize deciderSize mm = do
  let n = fromIntegral $ 3*classifierSize*classifierSize
  let app = stripedImage w h
  imgs <- replicateM n (randomImage w h)
  randomWain wainName app classifierSize imgs deciderSize mm

data Config = Config
  { universe :: CachedUniverse Astronomer,
    statsFile :: FilePath,
    sleepBetweenTasks :: Int,
    imageDB :: ImageDB,
    imageHeight :: Int,
    imageWidth :: Int,
    initialPopulationMaxClassifierSize :: Word8,
    initialPopulationMaxDeciderSize :: Word8,
    initialPopulationMaxAgeOfMaturity :: Word16,
    initialPopulationSize :: Int,
    baseMetabolismCost :: Double,
    childCostFactor :: Double,
    foragingIndex :: Int,
    maxPopulationSize :: Int,
    -- minCategories :: Int,
    maxCategories :: Int,
    maxSize :: Int,
    flirtingDeltaE :: Double,
    matingDeltaE :: Double,
    cooperationDeltaE :: Double,
    cooperationAgentAgreementDelta :: Double,
    cooperationImageAgreementDelta :: Double
  } deriving (Show, Eq)

data Summary = Summary
  {
    _rSizeDeltaE :: Double,
    _rChildRearingDeltaE :: Double,
    _rForagingDeltaE :: Double,
    _rCoopDeltaE :: Double,
    _rAgreementDeltaE :: Double,
    _rFlirtingDeltaE :: Double,
    _rMatingDeltaE :: Double,
    _rNetDeltaE :: Double,
    _otherDeltaE :: Double,
    _birthCount :: Int,
    _weanCount :: Int,
    _cooperateCount :: Int,
    _agreeCount :: Int,
    _flirtCount :: Int,
    _mateCount :: Int,
    _ignoreCount :: Int
  }
makeLenses ''Summary

initSummary :: Summary
initSummary = Summary
  {
    _rSizeDeltaE = 0,
    _rChildRearingDeltaE = 0,
    _rForagingDeltaE = 0,
    _rCoopDeltaE = 0,
    _rAgreementDeltaE = 0,
    _rFlirtingDeltaE = 0,
    _rMatingDeltaE = 0,
    _rNetDeltaE = 0,
    _otherDeltaE = 0,
    _birthCount = 0,
    _weanCount = 0,
    _cooperateCount = 0,
    _agreeCount = 0,
    _flirtCount = 0,
    _mateCount = 0,
    _ignoreCount = 0
  }

summaryStats :: Summary -> [Stats.Statistic]
summaryStats r =
  [
    Stats.uiStat "size Δe" (view rSizeDeltaE r),
    Stats.uiStat "child rearing Δe" (view rChildRearingDeltaE r),
    Stats.uiStat "foraging Δe" (view rForagingDeltaE r),
    Stats.uiStat "cooperation Δe" (view rCoopDeltaE r),
    Stats.uiStat "agreement Δe" (view rAgreementDeltaE r),
    Stats.uiStat "flirting Δe" (view rFlirtingDeltaE r),
    Stats.uiStat "mating Δe" (view rMatingDeltaE r),
    Stats.uiStat "net Δe" (view rNetDeltaE r),
    Stats.iStat "bore" (view birthCount r),
    Stats.iStat "weaned" (view weanCount r),
    Stats.iStat "co-operated" (view cooperateCount r),
    Stats.iStat "agreed" (view agreeCount r),
    Stats.iStat "flirted" (view flirtCount r),
    Stats.iStat "mated" (view mateCount r),
    Stats.iStat "ignored" (view ignoreCount r)
  ]

data Experiment = Experiment
  {
    _subject :: Astronomer,
    _directObject :: Object,
    _indirectObject :: Object,
    _weanlings :: [Astronomer],
    _config :: Config,
    _summary :: Summary
  }
makeLenses ''Experiment

run
  :: Config -> [Astronomer]
    -> StateT (CachedUniverse Astronomer) IO [Astronomer]
run cfg (me:xs) = do
  when (null xs) $ writeToLog "WARNING: Last wain standing!"
  (x, y) <- chooseObjects xs (imageDB cfg)
  let e = Experiment { _subject = me,
                       _directObject = x,
                       _indirectObject = y,
                       _weanlings = [],
                       _config = cfg,
                       _summary = initSummary }
  e' <- liftIO $ execStateT run' e
  let modifiedAgents = addIfAgent (view directObject e')
        . addIfAgent (view indirectObject e')
            $ (view subject e'):(view weanlings e')
  writeToLog $ "Modified agents: " ++ show (map agentId modifiedAgents)
  return modifiedAgents
run _ _ = error "no more wains"

run' :: StateT Experiment IO ()
run' = do
  a <- use subject
  withUniverse . writeToLog $ "---------- " ++ agentId a
    ++ "'s turn ----------"
  withUniverse . writeToLog $ "At beginning of turn, " ++ agentId a
    ++ "'s summary: " ++ pretty (Stats.stats a)
  forage
  (imgLabel, action) <- chooseAction'
  runAction action imgLabel
  adjustSubjectPassion
  when (hasLitter a) applyChildrearingCost
  wean
  applySizeCost
  incSubjectAge
  a' <- use subject
  withUniverse . writeToLog $ "End of " ++ agentId a ++ "'s turn"
  assign (summary.rNetDeltaE) (energy a' - energy a)
  sf <- fmap statsFile $ use config
  r <- fmap summaryStats $ use summary
  withUniverse . writeToLog $ "At end of turn, " ++ agentId a
    ++ "'s summary: " ++ pretty (Stats.stats a' ++ r)
  withUniverse $ updateStats r sf

applySizeCost :: StateT Experiment IO ()
applySizeCost = do
  a <- use subject
  ms <- fmap maxSize $ use config
  bms <- fmap baseMetabolismCost $ use config
  let deltaE = sizeCost ms bms a
  adjustSubjectEnergy deltaE rSizeDeltaE "size"

applyChildrearingCost :: StateT Experiment IO ()
applyChildrearingCost = do
  a <- use subject
  ccf <- fmap childCostFactor $ use config
  ms <- fmap maxSize $ use config
  bms <- fmap baseMetabolismCost $ use config
  let deltaE = childRearingCost ms bms ccf a
  adjustSubjectEnergy deltaE rChildRearingDeltaE "child rearing"

sizeCost :: Int -> Double -> Astronomer -> Double
sizeCost m b a = -(b + s/m')
  where s = fromIntegral (size a)
        m' = fromIntegral m

childRearingCost :: Int -> Double -> Double -> Astronomer -> Double
childRearingCost m b x a = x * (sum . map f $ litter a)
    where f c = sizeCost m b c

forage :: StateT Experiment IO ()
forage = do
  n <- withUniverse popSize
  mp <- fmap maxPopulationSize $ use config
  k <- fmap foragingIndex $ use config
  withUniverse . writeToLog $ "Pop. size=" ++ show n
  let deltaE = foragingReward mp n k
  adjustSubjectEnergy deltaE rForagingDeltaE "foraging"

foragingReward :: Int -> Int -> Int -> Double
foragingReward mp n k = (2*(0.5 - n'/mp'))^k
  where n' = fromIntegral n
        mp' = fromIntegral mp

schemaQuality :: Int -> Astronomer -> Double
schemaQuality m
  = schemaQuality' m . elems . counterMap . classifier . brain

schemaQuality' :: Integral a => Int -> [a] -> Double
schemaQuality' m xs = min 1 (n/xMax)
  where n = fromIntegral $ categoriesReallyUsed' m xs
        xMax = fromIntegral m
-- schemaQuality' xs = y*y
--   where y = 2*(x - 0.5)
--         x = (n - xMin)/(xMax - xMin)
--         n = min xMax . fromIntegral $ categoriesReallyUsed' xs
--         xMin = fromIntegral $ C.minCategories C.config
--         xMax = fromIntegral $ C.m C.config

categoriesReallyUsed :: Int -> Astronomer -> Int
categoriesReallyUsed m
  = categoriesReallyUsed' m . elems . counterMap
      . classifier . brain

categoriesReallyUsed' :: Integral a => Int -> [a] -> Int
categoriesReallyUsed' m xs = length $ filter (>k) xs
  where k = (sum xs) `div` (fromIntegral m)

chooseAction'
  :: StateT Experiment IO (Label, Action)
chooseAction' = do
  a <- use subject
  dObj <- use directObject
  iObj <- use indirectObject
  withUniverse . writeToLog $ agentId a ++ " sees " ++ objectId dObj
    ++ " and " ++ objectId iObj
  (imgLabel, action, a')
    <- withUniverse $
        chooseAction (objectAppearance dObj) (objectAppearance iObj) a
  withUniverse . writeToLog $ agentId a ++ " labels " ++ objectId dObj
    ++ " as " ++ show imgLabel ++ ", and chooses to "
    ++ describe (objectId dObj) (objectId iObj) action
  assign subject a'
  return (imgLabel, action)

incSubjectAge :: StateT Experiment IO ()
incSubjectAge = do
  a <- use subject
  a' <- withUniverse (incAge a)
  assign subject a'

describe :: String -> String -> Action -> String
describe _ iObj Cooperate = "share that classification with " ++ iObj
describe _ _    Flirt = "flirt"
describe _ _    Ignore = "do nothing"

chooseObjects
  :: [Astronomer] -> ImageDB
    -> StateT (CachedUniverse Astronomer) IO (Object, Object)
chooseObjects xs db = do
  -- withUniverse . writeToLog $ "Direct object = " ++ objectId x
  -- withUniverse . writeToLog $ "Indirect object = " ++ objectId y
  (x:y:_) <- liftIO . randomlyInsertImages db . map AObject $ xs
  return (x, y)

runAction :: Action -> Label -> StateT Experiment IO ()

--
-- Co-operate
--
runAction Cooperate aLabel = do
  applyCooperationEffects
  a <- use subject
  dObj <- use directObject
  iObj <- use indirectObject
  case iObj of
    AObject b   -> do
      withUniverse . writeToLog $ agentId a ++ " tells " ++ agentId b
        ++ " that image " ++ objectId dObj ++ " has label "
        ++ show aLabel
      let (bLabel, b') = classify (objectAppearance dObj) b
      assign directObject (AObject b')
      if aLabel == bLabel
        then agree aLabel
        else disagree aLabel bLabel
    IObject _ _ -> return ()
  
--
-- Flirt
--
runAction Flirt _ = do
  applyFlirtationEffects
  a <- use subject
  dObj <- use directObject
  withUniverse . writeToLog $
    agentId a ++ " flirts with " ++ objectId dObj
  if isImage dObj
    then return ()
    else flirt

--
-- Ignore
--
runAction Ignore _ = do
  a <- use subject
  dObj <- use directObject
  withUniverse . writeToLog $ agentId a ++ " ignores " ++ objectId dObj
  (summary.ignoreCount) += 1

-- --
-- -- Utility functions
-- --
agree :: Label -> StateT Experiment IO ()
agree label = do
  a <- use subject
  dObj <- use directObject
  (AObject b) <- use indirectObject
  let dObjApp = objectAppearance dObj
  withUniverse . writeToLog $ agentId b ++ " agrees with " ++  agentId a
    ++ " that " ++ objectId dObj ++ " has label " ++ show label
  a' <- withUniverse $ teachLabel dObjApp label a -- reinforce
  b' <- withUniverse $ teachLabel dObjApp label b -- reinforce
  assign subject a'
  assign directObject (AObject b')
  mc <- fmap maxCategories $ use config
  applyAgreementEffects mc

-- TODO: factor out common code in agree, disagree
  
disagree :: Label -> Label -> StateT Experiment IO ()
disagree aLabel bLabel = do
  a <- use subject
  dObj <- use directObject
  (AObject b) <- use indirectObject
  let dObjApp = objectAppearance dObj
  withUniverse . writeToLog $ agentId b ++ " disagrees with "
    ++ agentId a ++ ", says that " ++ objectId dObj ++ " has label "
    ++ show bLabel
  a' <- withUniverse $ teachLabel dObjApp bLabel a
  b' <- withUniverse $ teachLabel dObjApp aLabel b
  assign subject a'
  assign directObject (AObject b')

applyCooperationEffects :: StateT Experiment IO ()
applyCooperationEffects = do
  deltaE <- fmap cooperationDeltaE $ use config
  adjustSubjectEnergy deltaE rCoopDeltaE "cooperation"
  (summary.cooperateCount) += 1

applyAgreementEffects :: Int -> StateT Experiment IO ()
applyAgreementEffects mc = do
  b <- use indirectObject
  aa <- fmap cooperationAgentAgreementDelta $ use config
  ia <- fmap cooperationImageAgreementDelta $ use config
  sc <- fmap (schemaQuality mc) $ use subject
  let deltaE = if (isImage b) then ia*sc else aa*sc
  adjustSubjectEnergy deltaE rAgreementDeltaE "agreement"
  adjustObjectEnergy indirectObject deltaE "agreement"
  (summary.agreeCount) += 1

flirt :: StateT Experiment IO ()
flirt = do
  a <- use subject
  (AObject b) <- use directObject
  (a':b':_, mated) <- withUniverse (tryMating a b)
  if mated
    then do
      assign subject a'
      assign directObject (AObject b')
      recordBirths
      applyMatingEffects
    else return ()

recordBirths :: StateT Experiment IO ()
recordBirths = do
  a <- use subject
  (summary.birthCount) += length (litter a)

applyFlirtationEffects :: StateT Experiment IO ()
applyFlirtationEffects = do
  deltaE <- fmap flirtingDeltaE $ use config
  adjustSubjectEnergy deltaE rFlirtingDeltaE "flirting"
  (summary.flirtCount) += 1

applyMatingEffects :: StateT Experiment IO ()
applyMatingEffects = do
  deltaE <- fmap matingDeltaE $ use config
  adjustSubjectEnergy deltaE rMatingDeltaE "mating"
  adjustObjectEnergy directObject deltaE "mating"
  (summary.mateCount) += 1

wean :: StateT Experiment IO ()
wean = do
  (a:as) <- use subject >>= withUniverse . weanMatureChildren
  assign subject a
  assign weanlings as
  (summary.weanCount) += length as

withUniverse
  :: Monad m
    => StateT (CachedUniverse Astronomer) m a -> StateT Experiment m a
withUniverse f = do
  e <- get
  let c = view config e
  stateMap (\u -> set config (c{universe=u}) e) (universe . view config) f

finishRound :: FilePath -> StateT (CachedUniverse Astronomer) IO ()
finishRound f = do
  xs <- readStats f
  summarise xs
  clearStats f

adjustSubjectEnergy
  :: Double -> Simple Lens Summary Double -> String
    -> StateT Experiment IO ()
adjustSubjectEnergy deltaE selector reason = do
  x <- use subject
  let before = energy x
  assign (summary . selector) deltaE
  assign subject (adjustEnergy deltaE x)
  after <- fmap energy $ use subject
  withUniverse . writeToLog $ "Adjusting energy of " ++ agentId x
    ++ " because of " ++ reason
    ++ ". " ++ printf "%.3f" before ++ " + " ++ printf "%.3f" deltaE
    ++ " = " ++ printf "%.3f" after

adjustObjectEnergy
  :: Simple Lens Experiment Object -> Double -> String
    -> StateT Experiment IO ()
adjustObjectEnergy objectSelector deltaE reason = do
  x <- use objectSelector
  case x of
    AObject a -> do
      let before = energy a
      (summary . otherDeltaE) += deltaE
      let a' = adjustEnergy deltaE a
      let after = energy a'
      assign objectSelector (AObject a')
      withUniverse . writeToLog $ "Adjusting energy of " ++ agentId a
        ++ " because of " ++ reason
        ++ ". " ++ printf "%.3f" before ++ " + " ++ printf "%.3f" deltaE
        ++ " = " ++ printf "%.3f" after
    IObject _ _ -> return ()

adjustSubjectPassion
  :: StateT Experiment IO ()
adjustSubjectPassion = do
  x <- use subject
  assign subject (adjustPassion x)

