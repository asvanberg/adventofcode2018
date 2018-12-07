{-# LANGUAGE FlexibleContexts #-}
module Day1.Problem2 where

import Control.Monad.IO.Class
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set

import Day1.Problem1 (frequencyChanges, iif)

solution :: (MonadIO m) => m Int
solution = do
  deltas <- frequencyChanges "src/Day1/input"
  --evalStateT (duplicateFrequency (cycle deltas) 0) Set.empty
  return $ duplicateFrequency3 (cycle deltas) Set.empty 0

duplicateFrequency :: (MonadState (Set Int) m) => [Int] -> Int -> m Int
duplicateFrequency (delta:deltas) current = do
  seen <- get
  let newFrequency = current + delta
  if Set.member newFrequency seen
    then return newFrequency
    else do
      modify (Set.insert newFrequency)
      duplicateFrequency deltas newFrequency

duplicateFrequency2 :: (MonadState (Set Int) m) => [Int] -> Int -> m Int
duplicateFrequency2 (delta:deltas) current = do
  let newFrequency = current + delta
  Set.member newFrequency <$> get >>= iif
    (return newFrequency)
    (modify (Set.insert newFrequency) *> duplicateFrequency2 deltas newFrequency)

duplicateFrequency3 :: [Int] -> Set Int -> Int -> Int
duplicateFrequency3 (delta:deltas) seen current =
  let newFrequency = current + delta
  in if Set.member newFrequency seen
    then newFrequency
    else duplicateFrequency3 deltas (Set.insert newFrequency seen) newFrequency
