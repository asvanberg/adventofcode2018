{-# LANGUAGE FlexibleContexts #-}
module Day1.Problem2 where

import Control.Applicative (Alternative)
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Bifunctor
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO hiding (isEOF)

import Day1.Problem1 (isEOF, readNumber)

problem2 :: IO ()
problem2 = do
  h <- openFile "src/Day1/input" ReadMode
  deltas <- frequencyChanges h
  f <- evalStateT (duplicateFrequency (cycle deltas)) (Set.empty, 0)
  hClose h
  print f

duplicateFrequency :: (MonadState (Set Int, Int) m, MonadIO m) => [Int] -> m Int
duplicateFrequency (delta:deltas) = do
  (seen, current) <- get
  let newFrequency = current + delta
  if Set.member newFrequency seen
    then return newFrequency
    else do
      put (Set.insert newFrequency seen, newFrequency)
      duplicateFrequency deltas

frequencyChanges :: (MonadIO m) => Handle -> m [Int]
frequencyChanges handle = loop []
  where
    loop l = do
      eof <- isEOF handle
      if eof
        then return $ reverse l
        else do
          number <- readNumber handle
          loop (number : l)
