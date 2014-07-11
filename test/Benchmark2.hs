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

imageBenchmark :: Image -> IO ()
imageBenchmark img = do
  let imgs = replicate 100 img
  mapM_ imageBenchmark' imgs

imageBenchmark' :: Image -> IO ()
imageBenchmark' img = do
  let x = encode img
  (decode x :: Either String Image) `seq` return ()

runBenchmark :: String -> IO () -> IO ()
runBenchmark s b = do
  start <- getCurrentTime
  b
  end <- getCurrentTime
  putStrLn $ s ++ " took " ++ show (diffUTCTime end start)

prep :: IO ()
prep = do
  putStrLn "WARNING: Generating image for test. Run this a second time to get accurate results."
  img <- readImage $ "/home/amy/GalaxyZoo/table2/tiny-images/587742903940153520.jpeg"
  writeAstronomer testImageFilename w
  
main :: IO ()
main = do
  putStrLn $ "creatur-realtra v" ++ showVersion version
  putStrLn ALife.Creatur.Wain.programVersion
  putStrLn ALife.Creatur.programVersion
  img <- readImage $ "/home/amy/GalaxyZoo/table2/tiny-images/587742903940153520.jpeg"
  runBenchmark "imageBenchmark" (imageBenchmark img)
