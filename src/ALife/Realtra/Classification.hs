------------------------------------------------------------------------
-- |
-- Module      :  ALife.Realtra.Classification
-- Copyright   :  (c) Amy de Buitléir 2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Quality metrics for classification schema
--
------------------------------------------------------------------------
module ALife.Realtra.Classification
  (
    discrimination,
    novelty
  ) where

import Data.Maybe (fromMaybe)

discrimination :: Integral a => [a] -> Int
discrimination xs = length $ filter (>k) xs
  where k = (sum xs) `div` (fromIntegral $ 2 * length xs)

novelty :: (Eq a, Integral b) => a -> [(a, b)] -> Double
novelty l m = 1/(fromIntegral n)
  where n = fromMaybe 0 $ lookup l m
