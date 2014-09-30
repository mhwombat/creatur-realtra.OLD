------------------------------------------------------------------------
-- |
-- Module      :  ALife.Realtra.Photo
-- Copyright   :  (c) Amy de Buitléir 2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts, NoMonomorphismRestriction, ScopedTypeVariables #-}
module Main where

import ALife.Creatur.Universe
import ALife.Creatur.Wain
import qualified ALife.Realtra.Config as Config
import ALife.Realtra.Wain
import ALife.Realtra.Image
import Control.Monad.State
import Data.Colour.SRGB
import Data.Word
import Diagrams.Backend.Cairo
import Diagrams.Prelude
import System.Environment

grey2colour :: Word8 -> Colour Double
grey2colour x = sRGB x' x' x'
  where x' = (fromIntegral x)/255

colour2square
  :: (HasStyle b, Transformable b, TrailLike b, V b ~ R2)
    => Colour Double -> b
colour2square c = square 0.1 # fc c # lw none

imageRow
  :: (HasOrigin c, Juxtaposable c, HasStyle c, Transformable c,
    TrailLike c, Semigroup c, Monoid c, V c ~ R2)
      => [Word8] -> c
imageRow = hcat . map (colour2square . grey2colour)

image2diagram
  :: (HasOrigin c, Juxtaposable c, HasStyle c, TrailLike c,
    Transformable c, Semigroup c, Monoid c, V c ~ R2)
     => Image -> c
image2diagram = vcat . map imageRow . pixelArray

cream :: (Ord b, Floating b) => Colour b
cream = sRGB24 255 255 224

getWain
  :: (Universe u, Agent u ~ Astronomer)
    => String -> StateT u IO Astronomer
getWain s = do
  a <- getAgent s
  case a of
    (Right agent) -> return agent
    (Left msg)    -> error msg 
  
getWainName :: IO String
getWainName = do
  args <- getArgs
  if null args
    then error "Need to supply a wain name!"
    else return (head args)

main :: IO ()
main = do
  n <- getWainName
  w <- evalStateT (getWain n) (universe Config.config)
  let ss = mkSizeSpec (Just 500) Nothing
  let diagram = image2diagram . appearance $ w :: Diagram B R2
  let outputFileName = n ++ ".svg"
  renderCairo outputFileName ss diagram
