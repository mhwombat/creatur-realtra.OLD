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
import ALife.Creatur.Wain.GeneticSOM
import qualified ALife.Realtra.Config as Config
import ALife.Realtra.Wain
import ALife.Realtra.Image
import Control.Monad.State
import Data.Colour.SRGB
import Data.Function
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

-- grey2colour :: Word8 -> AlphaColour Double
-- grey2colour n = opaque $ sRGB24 (255 - n) (255 - n) (255 - n)
 
-- image2raster :: Image -> DImage Embedded
-- image2raster img = raster f (iWidth img) (iHeight img)
--   where f r c = grey2colour (pixelAt img r c)
-- -- raster :: (Int -> Int -> AlphaColour Double) -> Int -> Int -> DImage Embedded

cream :: (Ord b, Floating b) => Colour b
cream = sRGB24 255 255 224

-- drawHexagon
--   :: (Renderable Text b, Renderable (Path R2) b, Backend b R2)
--     => ((Int, Int), Image) -> Diagram b R2
drawHexagon
  :: (Renderable (DImage Embedded) b, Renderable Text b,
    Renderable (Path R2) b, Backend b R2)
    => ((Int, Int), Image) -> Diagram b R2
drawHexagon (index, img) = label `atop` pic `atop` hex
  where hex = hexagon 2 # lw (Local 0.05) # fc cream # rotateBy (1/4)
        label = translateY (1.3) $ text (show index) # fc black # fontSize (Local 0.4)
        pic = translateY 1 . translateX (-1) $ image2diagram img

drawHexRow
  :: (Backend b R2, Renderable (DImage Embedded) b,
    Renderable (Path R2) b, Renderable Text b)
     => [((Int, Int), Image)] -> Diagram b R2
drawHexRow = hcat . map drawHexagon

drawClassifier
  :: (Backend b R2, Renderable (DImage Embedded) b,
    Renderable (Path R2) b, Renderable Text b)
     => [((Int, Int), Image)] -> Diagram b R2
drawClassifier = mconcat . zipWith translateY [0,-3..] . map (centerX . drawHexRow) . rows
  -- where rs = rows xs
  --       w = maximum . map length $ rs
  --       f r = strutX . (4*) . fromIntegral $ w - length r
  --       spacers = map f rs

rows :: [((Int, Int), Image)] -> [[((Int, Int), Image)]]
rows = groupBy ((==) `on` (snd . fst)) . sortBy rowColumnOrder

rowColumnOrder :: ((Int, Int), a) -> ((Int, Int), a) -> Ordering
rowColumnOrder ((x1, y1), _) ((x2, y2), _)
  | y1 < y2   = GT
  | y1 > y2   = LT
  | x1 < x2   = LT
  | x1 > x2   = GT
  | otherwise = EQ

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
  
-- examine :: Astronomer -> IO ()
-- examine a = do
--   putStrLn $ "name: " ++ show (name a)
--   print (rows . toList . classifier . brain $ a)

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
  -- examine w
  let ss = mkSizeSpec (Just 500) Nothing
  -- let ps = take 441 . map round $ ([0,0.5..] :: [Double])
  -- let diagram = drawHexagon ((0,0),Image 21 21 ps ) :: Diagram B R2
  let diagram = drawClassifier . toList . classifier . brain $ w :: Diagram B R2
  let outputFileName = n ++ ".svg"
  renderSVG outputFileName ss diagram
  -- mainWith (circle 1 :: Diagram B R2)
  -- defaultMain (circle 1 :: Diagram B R2)
