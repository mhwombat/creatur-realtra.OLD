------------------------------------------------------------------------
-- |
-- Module      :  ALife.Realtra.Haar
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
import Codec.Haar
import qualified Data.ByteString as BS
import Data.Serialize
import System.Environment

main :: IO ()
main = do
  (infile:outfile:_) <- getArgs
  img <- readImage infile
  let pss = pixelArray img
  let pss2 = map (map fromIntegral) pss :: [[Double]]
  let pss3 = haar2D pss2
  BS.writeFile outfile $ encode pss3
