module Day2.Problem2 where

import           Control.Applicative
import           Data.List           (find)

solution :: IO (Maybe String)
solution = do
  xs <- lines <$> readFile "src/Day2/input"
  return $ findSimilar xs

findSimilar :: [String] -> Maybe String
findSimilar [] = Nothing
findSimilar (x:xs) =
  find ((== 25) . length) (map (common x) xs) <|> findSimilar xs

diff :: Eq a => [a] -> [a] -> [Bool]
diff a b = [x' == y' | (x', y') <- zip a b]

common :: Eq a => [a] -> [a] -> [a]
common a b = [x | (x, p) <- zip a (diff a b), p]
