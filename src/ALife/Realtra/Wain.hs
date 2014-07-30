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
import ALife.Creatur.Universe (Universe, Agent, writeToLog,
  withdrawEnergy)
import ALife.Creatur.Util (stateMap)
import ALife.Creatur.Wain (Wain(..), Label, adjustEnergy, adjustPassion,
  chooseAction, buildWainAndGenerateGenome, classify, teachLabel,
  incAge, weanMatureChildren, tryMating, energy, passion, hasLitter,
  reflect)
import ALife.Creatur.Wain.Brain (classifier, buildBrain)
import ALife.Creatur.Wain.GeneticSOM (RandomDecayingGaussianParams(..),
  randomDecayingGaussian, buildGeneticSOM, numModels, counterMap)
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.Response (Response, randomResponse, action)
import ALife.Creatur.Wain.Util (unitInterval)
import qualified ALife.Creatur.Wain.Statistics as Stats
import ALife.Realtra.Action (Action(..))
import ALife.Creatur.Wain.PersistentStatistics (updateStats, readStats,
  clearStats, summarise)
import ALife.Realtra.Image (Image, stripedImage, randomImage)
import ALife.Realtra.ImageDB (ImageDB, anyImage)
import Control.Lens hiding (Action, universe)
import Control.Monad (replicateM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (Rand, RandomGen, getRandomR)
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
    => String -> Config u -> Word8 -> Word8 -> Rand r Astronomer
randomAstronomer wainName config classifierSize deciderSize = do
  let n = fromIntegral $ 3*classifierSize*classifierSize
  let w = imageWidth config
  let h = imageWidth config
  imgs <- replicateM n (randomImage w h)
  -- let fcp = randomDecayingGaussianParams classifierSize
  let fcp = RandomDecayingGaussianParams
               { r0Range = classifierR0Range config,
                 rfRange = classifierRfRange config,
                 w0Range = classifierW0Range config,
                 wfRange = classifierWfRange config,
                 tfRange = classifierTfRange config,
                 sideLength = classifierSize }
  fc <- randomDecayingGaussian fcp
  let c = buildGeneticSOM classifierSize fc imgs
  -- let fdp = randomDecayingGaussianParams deciderSize
  let fdp = RandomDecayingGaussianParams
              { r0Range = deciderR0Range config,
                rfRange = deciderRfRange config,
                w0Range = deciderW0Range config,
                wfRange = deciderWfRange config,
                tfRange = deciderTfRange config,
                sideLength = deciderSize }
  fd <- randomDecayingGaussian fdp
  xs <- replicateM (numTiles . initialPopulationMaxDeciderSize $ config)
         $ randomResponse (numModels c) 
  let d = buildGeneticSOM deciderSize fd xs
  let b = buildBrain c d
  m <- getRandomR (0,initialPopulationMaxAgeOfMaturity config)
  p <- getRandomR unitInterval
  let app = stripedImage w h
  return $ buildWainAndGenerateGenome wainName app b m p

numTiles :: Word8 -> Int
numTiles s = 3*s'*(s'-1) + 1
  where s' = fromIntegral s

data Config u = Config
  { universe :: u,
    statsFile :: FilePath,
    sleepBetweenTasks :: Int,
    imageDB :: ImageDB,
    imageWidth :: Int,
    imageHeight :: Int,
    initialPopulationMaxClassifierSize :: Word8,
    initialPopulationMaxDeciderSize :: Word8,
    initialPopulationMaxAgeOfMaturity :: Word16,
    initialPopulationSize :: Int,
    minPopulationSize :: Int,
    maxPopulationSize :: Int,
    energyPoolSize :: Double,
    baseMetabolismDeltaE :: Double,
    energyCostPerByte :: Double,
    childCostFactor :: Double,
    -- minCategories :: Int,
    maxCategories :: Int,
    flirtingDeltaE :: Double,
    matingDeltaE :: Double,
    cooperationDeltaE :: Double,
    cooperationAgentAgreementDelta :: Double,
    cooperationImageAgreementDelta :: Double,
    classifierR0Range :: (Double,Double),
    classifierRfRange :: (Double,Double),
    classifierW0Range :: (Double,Double),
    classifierWfRange :: (Double,Double),
    classifierTfRange :: (Double,Double),
    deciderR0Range :: (Double,Double),
    deciderRfRange :: (Double,Double),
    deciderW0Range :: (Double,Double),
    deciderWfRange :: (Double,Double),
    deciderTfRange :: (Double,Double)
  } deriving (Show, Eq)

data Summary = Summary
  {
    _rSchemaQuality :: Double,
    _rSizeDeltaE :: Double,
    _rChildRearingDeltaE :: Double,
    -- _rForagingDeltaE :: Double,
    _rCoopDeltaE :: Double,
    _rAgreementDeltaE :: Double,
    _rFlirtingDeltaE :: Double,
    _rMatingDeltaE :: Double,
    _rNetDeltaE :: Double,
    _rOtherDeltaE :: Double,
    _rErr :: Double,
    _rBirthCount :: Int,
    _rWeanCount :: Int,
    _rCooperateCount :: Int,
    _rAgreeCount :: Int,
    _rFlirtCount :: Int,
    _rMateCount :: Int,
    _rIgnoreCount :: Int
  }
makeLenses ''Summary

initSummary :: Summary
initSummary = Summary
  {
    _rSchemaQuality = 0,
    _rSizeDeltaE = 0,
    _rChildRearingDeltaE = 0,
    -- _rForagingDeltaE = 0,
    _rCoopDeltaE = 0,
    _rAgreementDeltaE = 0,
    _rFlirtingDeltaE = 0,
    _rMatingDeltaE = 0,
    _rNetDeltaE = 0,
    _rOtherDeltaE = 0,
    _rErr = 0,
    _rBirthCount = 0,
    _rWeanCount = 0,
    _rCooperateCount = 0,
    _rAgreeCount = 0,
    _rFlirtCount = 0,
    _rMateCount = 0,
    _rIgnoreCount = 0
  }

summaryStats :: Summary -> [Stats.Statistic]
summaryStats r =
  [
    Stats.uiStat "SQ" (view rSchemaQuality r),
    Stats.uiStat "size Δe" (view rSizeDeltaE r),
    Stats.uiStat "child rearing Δe" (view rChildRearingDeltaE r),
    -- Stats.uiStat "foraging Δe" (view rForagingDeltaE r),
    Stats.uiStat "cooperation Δe" (view rCoopDeltaE r),
    Stats.uiStat "agreement Δe" (view rAgreementDeltaE r),
    Stats.uiStat "flirting Δe" (view rFlirtingDeltaE r),
    Stats.uiStat "mating Δe" (view rMatingDeltaE r),
    Stats.uiStat "net Δe" (view rNetDeltaE r),
    Stats.uiStat "other Δe" (view rOtherDeltaE r),
    Stats.uiStat "err" (view rErr r),
    Stats.iStat "bore" (view rBirthCount r),
    Stats.iStat "weaned" (view rWeanCount r),
    Stats.iStat "co-operated" (view rCooperateCount r),
    Stats.iStat "agreed" (view rAgreeCount r),
    Stats.iStat "flirted" (view rFlirtCount r),
    Stats.iStat "mated" (view rMateCount r),
    Stats.iStat "ignored" (view rIgnoreCount r)
  ]

data Experiment u = Experiment
  {
    _subject :: Astronomer,
    _directObject :: Object,
    _indirectObject :: Object,
    _weanlings :: [Astronomer],
    _config :: Config u,
    _summary :: Summary
  }
makeLenses ''Experiment

run
  :: (Universe u, Agent u ~ Astronomer)
    => Config u -> [Astronomer] -> StateT u IO [Astronomer]
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

run' :: (Universe u, Agent u ~ Astronomer) => StateT (Experiment u) IO ()
run' = do
  a <- use subject
  withUniverse . writeToLog $ "---------- " ++ agentId a
    ++ "'s turn ----------"
  withUniverse . writeToLog $ "At beginning of turn, " ++ agentId a
    ++ "'s summary: " ++ pretty (Stats.stats a)
  -- forage
  (imgLabel, r) <- chooseAction'
  runAction (action r) imgLabel
  letSubjectReflect r
  adjustSubjectPassion
  when (hasLitter a) applyChildrearingCost
  wean
  applySizeCost
  incSubjectAge
  a' <- use subject
  withUniverse . writeToLog $ "End of " ++ agentId a ++ "'s turn"
  assign (summary.rNetDeltaE) (energy a' - energy a)
  sf <- fmap statsFile $ use config
  agentStats <- fmap ((Stats.stats a' ++) . summaryStats) $ use summary
  withUniverse . writeToLog $ "At end of turn, " ++ agentId a
    ++ "'s summary: " ++ pretty agentStats
  withUniverse $ updateStats agentStats sf

applySizeCost
  :: (Universe u, Agent u ~ Astronomer)
    => StateT (Experiment u) IO ()
applySizeCost = do
  a <- use subject
  bms <- fmap baseMetabolismDeltaE $ use config
  cps <- fmap energyCostPerByte $ use config
  let deltaE = sizeCost bms cps a
  adjustSubjectEnergy deltaE rSizeDeltaE "size"

applyChildrearingCost
  :: (Universe u, Agent u ~ Astronomer)
    => StateT (Experiment u) IO ()
applyChildrearingCost = do
  a <- use subject
  ccf <- fmap childCostFactor $ use config
  bms <- fmap baseMetabolismDeltaE $ use config
  cps <- fmap energyCostPerByte $ use config
  let deltaE = childRearingCost bms cps ccf a
  adjustSubjectEnergy deltaE rChildRearingDeltaE "child rearing"

sizeCost :: Double -> Double -> Astronomer -> Double
sizeCost b f a = b + f*s
  where s = fromIntegral (size a)

childRearingCost :: Double -> Double -> Double -> Astronomer -> Double
childRearingCost b f x a = x * (sum . map g $ litter a)
    where g c = sizeCost b f c

-- forage :: (Universe u, Agent u ~ Astronomer) => StateT (Experiment u) IO ()
-- forage = do
--   n <- withUniverse popSize
--   mp <- fmap maxPopulationSize $ use config
--   k <- fmap foragingIndex $ use config
--   withUniverse . writeToLog $ "Pop. size=" ++ show n
--   let deltaE = foragingReward mp n k
--   adjustSubjectEnergy deltaE rForagingDeltaE "foraging"

-- foragingReward :: Int -> Int -> Int -> Double
-- foragingReward mp n k = (2*(0.5 - n'/mp'))^k
--   where n' = fromIntegral n
--         mp' = fromIntegral mp

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
  :: (Universe u, Agent u ~ Astronomer)
    => StateT (Experiment u) IO (Label, Response Action)
chooseAction' = do
  a <- use subject
  dObj <- use directObject
  iObj <- use indirectObject
  withUniverse . writeToLog $ agentId a ++ " sees " ++ objectId dObj
    ++ " and " ++ objectId iObj
  let (imgLabel, _) = classify (objectAppearance dObj) a
  (r, a')
    <- withUniverse $
        chooseAction (objectAppearance dObj) (objectAppearance iObj) a
  withUniverse . writeToLog $ agentId a ++ " labels " ++ objectId dObj
    ++ " as " ++ show imgLabel ++ ", and chooses to "
    ++ describe (objectId dObj) (objectId iObj) (action r)
  assign subject a'
  return (imgLabel, r)

incSubjectAge
  :: (Universe u, Agent u ~ Astronomer)
    => StateT (Experiment u) IO ()
incSubjectAge = do
  a <- use subject
  a' <- withUniverse (incAge a)
  assign subject a'

describe :: String -> String -> Action -> String
describe _ iObj Cooperate = "share that classification with " ++ iObj
describe _ _    Flirt = "flirt"
describe _ _    Ignore = "do nothing"

chooseObjects
  :: (Universe u, Agent u ~ Astronomer)
    => [Astronomer] -> ImageDB -> StateT u IO (Object, Object)
chooseObjects xs db = do
  -- withUniverse . writeToLog $ "Direct object = " ++ objectId x
  -- withUniverse . writeToLog $ "Indirect object = " ++ objectId y
  (x:y:_) <- liftIO . randomlyInsertImages db . map AObject $ xs
  return (x, y)

runAction
  :: (Universe u, Agent u ~ Astronomer)
    => Action -> Label -> StateT (Experiment u) IO ()

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
      assign indirectObject (AObject b')
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
  (summary.rIgnoreCount) += 1

-- --
-- -- Utility functions
-- --
agree
  :: (Universe u, Agent u ~ Astronomer)
    => Label -> StateT (Experiment u) IO ()
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
  assign indirectObject (AObject b')
  mc <- fmap maxCategories $ use config
  applyAgreementEffects mc

-- TODO: factor out common code in agree, disagree
  
disagree
  :: (Universe u, Agent u ~ Astronomer)
    => Label -> Label -> StateT (Experiment u) IO ()
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
  assign indirectObject (AObject b')

applyCooperationEffects
  :: (Universe u, Agent u ~ Astronomer)
    => StateT (Experiment u) IO ()
applyCooperationEffects = do
  deltaE <- fmap cooperationDeltaE $ use config
  adjustSubjectEnergy deltaE rCoopDeltaE "cooperation"
  (summary.rCooperateCount) += 1

applyAgreementEffects
  :: (Universe u, Agent u ~ Astronomer)
    => Int -> StateT (Experiment u) IO ()
applyAgreementEffects mc = do
  b <- use directObject
  aa <- fmap cooperationAgentAgreementDelta $ use config
  ia <- fmap cooperationImageAgreementDelta $ use config
  sc <- fmap (schemaQuality mc) $ use subject
  let deltaE = if (isImage b) then ia*sc else aa*sc
  let reason = if (isImage b)
                 then "agreed about image"
                 else "agreed about agent"
  adjustSubjectEnergy deltaE rAgreementDeltaE reason
  adjustObjectEnergy indirectObject deltaE reason
  (summary.rAgreeCount) += 1

flirt :: (Universe u, Agent u ~ Astronomer) => StateT (Experiment u) IO ()
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

recordBirths :: StateT (Experiment u) IO ()
recordBirths = do
  a <- use subject
  (summary.rBirthCount) += length (litter a)

applyFlirtationEffects
  :: (Universe u, Agent u ~ Astronomer)
    => StateT (Experiment u) IO ()
applyFlirtationEffects = do
  deltaE <- fmap flirtingDeltaE $ use config
  adjustSubjectEnergy deltaE rFlirtingDeltaE "flirting"
  (summary.rFlirtCount) += 1

applyMatingEffects
  :: (Universe u, Agent u ~ Astronomer)
    => StateT (Experiment u) IO ()
applyMatingEffects = do
  deltaE <- fmap matingDeltaE $ use config
  adjustSubjectEnergy deltaE rMatingDeltaE "mating"
  adjustObjectEnergy directObject deltaE "mating"
  (summary.rMateCount) += 1

wean :: (Universe u, Agent u ~ Astronomer) => StateT (Experiment u) IO ()
wean = do
  (a:as) <- use subject >>= withUniverse . weanMatureChildren
  assign subject a
  assign weanlings as
  (summary.rWeanCount) += length as

withUniverse
  :: (Universe u, Agent u ~ Astronomer, Monad m)
    => StateT u m a -> StateT (Experiment u) m a
withUniverse f = do
  e <- get
  let c = view config e
  stateMap (\u -> set config (c{universe=u}) e) (universe . view config) f

finishRound
  :: (Universe u, Agent u ~ Astronomer)
    => FilePath -> StateT u IO ()
finishRound f = do
  xs <- readStats f
  summarise xs
  clearStats f

adjustSubjectEnergy
  :: (Universe u, Agent u ~ Astronomer)
    => Double -> Simple Lens Summary Double -> String
      -> StateT (Experiment u) IO ()
adjustSubjectEnergy deltaE selector reason = do
  x <- use subject
  let before = energy x
  deltaE' <- adjustedDeltaE deltaE
  assign (summary . selector) deltaE'
  assign subject (adjustEnergy deltaE' x)
  after <- fmap energy $ use subject
  withUniverse . writeToLog $ "Adjusting energy of " ++ agentId x
    ++ " because of " ++ reason
    ++ ". " ++ printf "%.3f" before ++ " + " ++ printf "%.3f" deltaE'
    ++ " = " ++ printf "%.3f" after

adjustObjectEnergy
  :: (Universe u, Agent u ~ Astronomer)
    => Simple Lens (Experiment u) Object -> Double -> String
      -> StateT (Experiment u) IO ()
adjustObjectEnergy objectSelector deltaE reason = do
  x <- use objectSelector
  case x of
    AObject a -> do
      let before = energy a
      deltaE' <- adjustedDeltaE deltaE
      (summary . rOtherDeltaE) += deltaE'
      let a' = adjustEnergy deltaE' a
      let after = energy a'
      assign objectSelector (AObject a')
      withUniverse . writeToLog $ "Adjusting energy of " ++ agentId a
        ++ " because of " ++ reason
        ++ ". " ++ printf "%.3f" before ++ " + "
        ++ printf "%.3f" deltaE' ++ " = " ++ printf "%.3f" after
    IObject _ _ -> return ()

adjustedDeltaE
  :: (Universe u, Agent u ~ Astronomer)
    => Double -> StateT (Experiment u) IO Double
adjustedDeltaE deltaE =
  if deltaE <= 0
    then return deltaE
    else do
      deltaE' <- withUniverse (withdrawEnergy deltaE)
      when (deltaE' < deltaE) $ do
        withUniverse . writeToLog $ "Energy pool exhausted, only "
          ++ show deltaE' ++ " available"
      return deltaE'

adjustSubjectPassion
  :: StateT (Experiment u) IO ()
adjustSubjectPassion = do
  x <- use subject
  assign subject (adjustPassion x)

letSubjectReflect
  :: (Universe u, Agent u ~ Astronomer)
    => Response Action -> StateT (Experiment u) IO ()
letSubjectReflect r = do
  x <- use subject
  p1 <- fmap objectAppearance $ use directObject
  p2 <- fmap objectAppearance $ use indirectObject
  (x', err) <- withUniverse (reflect p1 p2 r x)
  assign subject x'
  assign (summary . rErr) err
