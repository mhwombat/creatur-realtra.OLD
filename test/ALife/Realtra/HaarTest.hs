------------------------------------------------------------------------
-- |
-- Module      :  ALife.Realtra.HaarTest
-- Copyright   :  (c) Amy de Buitléir 2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
module Main where

import qualified Data.ByteString as BS
import Data.Datamining.Pattern
import Data.Serialize

haarify :: FilePath -> IO [Double]
haarify filepath = do
  (Right pss) <- fmap decode $ BS.readFile filepath
                 :: IO (Either String [[Double]])
  putStrLn $ filepath ++ " Haar transform:"
  mapM_ print pss
  return $ concat pss

main :: IO ()
main = do
  xss1 <- haarify "img1.haar"
  xss1_10 <- haarify "img1_10.haar"
  xss1_20 <- haarify "img1_20.haar"
  xss1_30 <- haarify "img1_30.haar"
  xss1_40 <- haarify "img1_40.haar"
  xss1_50 <- haarify "img1_50.haar"
  xss1_60 <- haarify "img1_60.haar"
  xss1_70 <- haarify "img1_70.haar"
  xss1_80 <- haarify "img1_80.haar"
  xss1_90 <- haarify "img1_90.haar"
  xss1_120 <- haarify "img1_120.haar"
  xss1_180 <- haarify "img1_180.haar"

  xss2 <- haarify "img2.haar"
  xss2a <- haarify "img2a.haar"
  xss2b <- haarify "img2b.haar"
  xss2c <- haarify "img2c.haar"

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
