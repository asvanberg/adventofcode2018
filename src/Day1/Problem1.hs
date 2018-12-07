module Day1.Problem1 where

import Control.Monad.IO.Class
import Data.List (isPrefixOf)

solution :: (MonadIO m) => m Int
solution = sum <$> frequencyChanges "src/Day1/input"

frequencyChanges :: (MonadIO m) => FilePath -> m [Int]
frequencyChanges = liftIO . fmap (fmap readNumber . lines) . readFile
  where
    readNumber = read . clean
    clean = iif tail id =<< isPrefixOf "+"

iif :: a -> a -> Bool -> a
iif t f b = if b then t else f
