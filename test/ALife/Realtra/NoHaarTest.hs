------------------------------------------------------------------------
-- |
-- Module      :  ALife.Realtra.NoHaarTest
-- Copyright   :  (c) Amy de Buitléir 2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
module Main where

import ALife.Realtra.Image
-- import qualified Data.ByteString as BS
import Data.Datamining.Pattern
-- import Data.Serialize

main :: IO ()
main = do
  xss1 <- readImage "img1.jpeg"
  xss1_10 <- readImage "img1_10.jpeg"
  xss1_20 <- readImage "img1_20.jpeg"
  xss1_30 <- readImage "img1_30.jpeg"
  xss1_40 <- readImage "img1_40.jpeg"
  xss1_50 <- readImage "img1_50.jpeg"
  xss1_60 <- readImage "img1_60.jpeg"
  xss1_70 <- readImage "img1_70.jpeg"
  xss1_80 <- readImage "img1_80.jpeg"
  xss1_90 <- readImage "img1_90.jpeg"
  xss1_120 <- readImage "img1_120.jpeg"
  xss1_180 <- readImage "img1_180.jpeg"

  xss2 <- readImage "img2.jpeg"
  xss2a <- readImage "img2a.jpeg"
  xss2b <- readImage "img2b.jpeg"
  xss2c <- readImage "img2c.jpeg"

  putStrLn $ "Diff btw img1 & img1_10: " ++ show (difference xss1 xss1_10)
  putStrLn $ "Diff btw img1 & img1_20: " ++ show (difference xss1 xss1_20)
  putStrLn $ "Diff btw img1 & img1_30: " ++ show (difference xss1 xss1_30)
  putStrLn $ "Diff btw img1 & img1_40: " ++ show (difference xss1 xss1_40)
  putStrLn $ "Diff btw img1 & img1_50: " ++ show (difference xss1 xss1_50)
  putStrLn $ "Diff btw img1 & img1_60: " ++ show (difference xss1 xss1_60)
  putStrLn $ "Diff btw img1 & img1_70: " ++ show (difference xss1 xss1_70)
  putStrLn $ "Diff btw img1 & img1_80: " ++ show (difference xss1 xss1_80)
  putStrLn $ "Diff btw img1 & img1_90: " ++ show (difference xss1 xss1_90)
  putStrLn $ "Diff btw img1 & img1_120: " ++ show (difference xss1 xss1_120)
  putStrLn $ "Diff btw img1 & img1_180: " ++ show (difference xss1 xss1_180)
  putStrLn $ "Diff btw img1 & img2: " ++ show (difference xss1 xss2)
  putStrLn $ "Diff btw img1 & img2a: " ++ show (difference xss1 xss2a)
  putStrLn $ "Diff btw img1 & img2b: " ++ show (difference xss1 xss2b)
  putStrLn $ "Diff btw img1 & img2c: " ++ show (difference xss1 xss2c)
