------------------------------------------------------------------------
-- |
-- Module      :  ALife.Realtra.Statistics
-- Copyright   :  (c) Amy de Buitléir 2012-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
module ALife.Realtra.Statistics
  (
    updateStats,
    readStats,
    clearStats,
    summarise
  ) where

import ALife.Creatur.Universe (Universe, writeToLog)
import qualified ALife.Creatur.Wain.Statistics as Stats
import qualified ALife.Realtra.Config as Config
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT)
import qualified Data.Serialize as DS
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath (dropFileName)
--import System.IO (hGetContents, withFile, Handle, IOMode(ReadMode))
--import Text.Read (readEither)
import qualified Data.ByteString as BS

updateStats :: Universe u => [Stats.Statistic] -> StateT u IO ()
updateStats x = do
  xs <- readStats
  writeStats Config.statsFile (x:xs)

clearStats :: Universe u => StateT u IO ()
clearStats = writeStats Config.statsFile []

readStats :: Universe u => StateT u IO [[Stats.Statistic]]
readStats = do
  let f = Config.statsFile
  fExists <- liftIO $ doesFileExist f
  if fExists
    then do
      b <- liftIO $ BS.readFile f
      let x = DS.decode b
      -- x <- liftIO $ withFile f ReadMode readStats' -- closes file ASAP
      case x of
        Left msg  -> do
          writeToLog $ "Unable to read stats from " ++ f ++ ": " ++ msg
          return []
        Right xs -> return xs
    else return []

-- readStats' :: Handle -> IO (Either String [[Statistic]])
-- readStats' h = do
--   s <- hGetContents h
--   let x = readEither s
--   case x of
--     Left msg -> return $ Left (msg ++ "\"" ++ s ++ "\"")
--     Right c  -> return $ Right c


writeStats :: Universe u => FilePath -> [[Stats.Statistic]] -> StateT u IO ()
writeStats f xs = do
  liftIO $ createDirectoryIfMissing True (dropFileName f)
  liftIO $ BS.writeFile f (DS.encode xs)

summarise :: Universe u => [[Stats.Statistic]] -> StateT u IO ()
summarise xss = mapM_ f $ Stats.summarise xss
  where f s = writeToLog $ "Summary - " ++ s
