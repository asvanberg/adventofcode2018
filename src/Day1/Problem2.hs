{-# LANGUAGE FlexibleContexts #-}
module Day1.Problem2 where

import Control.Monad.IO.Class
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set

import Day1.Problem1 (frequencyChanges)

solution :: (MonadIO m) => m Int
solution = do
  deltas <- frequencyChanges "src/Day1/input"
  evalStateT (duplicateFrequency (cycle deltas)) (Set.empty, 0)

duplicateFrequency :: (MonadState (Set Int, Int) m) => [Int] -> m Int
duplicateFrequency (delta:deltas) = do
  (seen, current) <- get
  let newFrequency = current + delta
  if Set.member newFrequency seen
    then return newFrequency
    else do
      put (Set.insert newFrequency seen, newFrequency)
      duplicateFrequency deltas
