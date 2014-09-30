------------------------------------------------------------------------
-- |
-- Module      :  ALife.Realtra.UnHaar
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

import ALife.Realtra.Image
import Codec.Haar
import qualified Data.ByteString as BS
import Data.Serialize
import System.Environment

main :: IO ()
main = do
  (infile:outfile:_) <- getArgs
  (Right pss) <- fmap decode $ BS.readFile infile
                 :: IO (Either String [[Double]])
  let pss2 = unHaar2D pss
  let pss3 = map (map round) pss2
  let h = length pss3
  let w = length (head pss3)
  let img = mkImage w h $ concat pss3
  writeImage outfile img
