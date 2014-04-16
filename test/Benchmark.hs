module Main where

import Control.Conditional (unlessM)
import Data.Time.Clock

import Prelude hiding (readFile, writeFile)
import ALife.Creatur
import ALife.Creatur.Wain hiding (size)
import ALife.Creatur.Wain.Brain
import ALife.Realtra.Wain
import ALife.Realtra.Image
import Control.Monad (replicateM)
import Control.Monad.Random (evalRandIO)
import Data.ByteString (readFile, writeFile)
import qualified Data.Serialize as DS 
import Data.Version (showVersion)
import Paths_creatur_realtra (version)
import System.Directory (doesFileExist)

testWainFilename :: FilePath
testWainFilename = "testWain"

wainBenchmark :: IO ()
wainBenchmark = do
  (Right w) <- readAstronomer testWainFilename
  img <- readImage $ "/home/amy/GalaxyZoo/table2/tiny-images/587742903940153520.jpeg"
  let b = brain w
  let (_, _, _, b') = assessSituation img img (condition w) b
  writeAstronomer testWainFilename (w {brain=b'})

readAstronomer :: FilePath -> IO (Either String Astronomer)
readAstronomer f = do
  x <- readFile f
  return $ DS.decode x

-- | Write a record to a file.
writeAstronomer :: FilePath -> Astronomer -> IO ()
writeAstronomer f a = do
  let x = DS.encode a
  writeFile f x

runBenchmark :: String -> IO () -> IO ()
runBenchmark s b = do
  start <- getCurrentTime
  b
  end <- getCurrentTime
  putStrLn $ s ++ " took " ++ show (diffUTCTime end start)

prep :: IO ()
prep = do
  putStrLn "WARNING: Generating wain for test. Run this a second time to get accurate results."
  let classifierSize = 3
  let deciderSize=3
  let n = fromIntegral (3*classifierSize*classifierSize)
  (app:ps) <- evalRandIO (replicateM n $ randomImage 21 21)
  w <- evalRandIO
        (randomWain "fred" app classifierSize ps deciderSize 100)
          :: IO Astronomer
  writeAstronomer testWainFilename w
  
main :: IO ()
main = do
  putStrLn $ "creatur-realtra v" ++ showVersion version
  putStrLn ALife.Creatur.Wain.programVersion
  putStrLn ALife.Creatur.programVersion
  unlessM (doesFileExist testWainFilename) prep
  runBenchmark "wainBenchmark" wainBenchmark

