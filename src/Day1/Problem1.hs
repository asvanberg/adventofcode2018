module Day1.Problem1 where

import Control.Monad.IO.Class
import Data.List (isPrefixOf)

problem1 :: IO ()
problem1 = do
  print =<< foldr (+) 0 <$> frequencyChanges "src/Day1/input"

frequencyChanges :: (MonadIO m) => FilePath -> m [Int]
frequencyChanges file = liftIO $ (fmap readNumber) <$> lines <$> readFile file
  where
    readNumber = read . clean
    clean = iif tail id =<< isPrefixOf "+"

iif :: a -> a -> Bool -> a
iif t f b = if b then t else f
