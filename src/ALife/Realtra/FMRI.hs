------------------------------------------------------------------------
-- |
-- Module      :  ALife.Realtra.FMRI
-- Copyright   :  (c) Amy de Buitléir 2013
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
import ALife.Creatur.Wain.Brain
-- import ALife.Creatur.Wain.Classifier
-- import ALife.Creatur.Wain.GeneticSOM
import qualified ALife.Realtra.Config as Config
import ALife.Realtra.Wain
import ALife.Realtra.Image
import Control.Monad.State
import Data.Colour.SRGB
import Data.List
-- import Data.List.Split
import Data.Word
import Diagrams.Backend.SVG
-- import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import Diagrams.TwoD.Text
-- import Numeric
import System.Environment
import Text.Printf (printf)

grey2colour :: Word8 -> AlphaColour Double
grey2colour n = opaque $ sRGB24 (255 - n) (255 - n) (255 - n)
 
image2raster :: Image -> DImage Embedded
image2raster img = raster f (iWidth img) (iHeight img)
  where f r c = grey2colour (pixelAt img r c)
-- raster :: (Int -> Int -> AlphaColour Double) -> Int -> Int -> DImage Embedded

-- drawHexagon
--   :: (Renderable Text b, Renderable (Path R2) b, Backend b R2)
--     => ((Int, Int), Image) -> Diagram b R2
drawHexagon
  :: (Renderable (DImage Embedded) b, Renderable Text b,
    Renderable (Path R2) b, Backend b R2)
    => ((Int, Int), Image) -> Diagram b R2
drawHexagon (index, img) =
  mconcat [ label,
            hexagon 2 # lw (Local 0.05) # fc blue # rotateBy (1/4) ]
    where label = (text (show index) # fc black # fontSize (Local 0.5) ||| strutY 1)
                  ===
                  image (image2raster img) # sized (Height 1)

-- classifierDiagram :: Classifier Image -> Diagram B R2
-- classifierDiagram =
--   vcat . map (visualiseDeciderModel . parseDeciderModel) . lines

getWain
  :: (Universe u, Agent u ~ Astronomer)
    => String -> StateT u IO Astronomer
getWain s = do
  a <- getAgent s
  case a of
    (Right agent) -> return agent
    (Left msg)    -> error msg 
  
examine :: Astronomer -> IO ()
examine a = do
  putStrLn $ "name: " ++ show (name a)
  print (classifier . brain $ a)

formatVector :: String -> [Double] -> String
formatVector fmt = intercalate " " . map (printf fmt)

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
  examine w
  let ss = mkSizeSpec (Just 500) Nothing
  let diagram = drawHexagon ((0,0),stripedImage 5 5) :: Diagram B R2
  -- let diagram = hexagon 20 # lw thin :: Diagram B R2
  renderSVG "amy.svg" ss diagram

-- main :: IO ()
-- main = do
--   -- mainWith (circle 1 :: Diagram B R2)
--   -- defaultMain (circle 1 :: Diagram B R2)
--   let ss = mkSizeSpec (Just 100) Nothing
--   renderSVG "amy.svg" ss (circle 1 :: Diagram B R2)
