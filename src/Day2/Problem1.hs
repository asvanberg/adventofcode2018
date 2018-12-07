module Day2.Problem1 where

import Data.Map hiding (filter)

solution :: IO Int
solution = do
  xs <- lines <$> readFile "src/Day2/input"
  let breakdowns = elems . breakdown <$> xs
      twos = count (2 `elem`) breakdowns
      threes = count (3 `elem`) breakdowns
  return $ twos * threes

breakdown :: String -> Map Char Int
breakdown =
  fromListWith (+) . flip zip (repeat 1)

count :: (a -> Bool) -> [a] -> Int
count = (length .) . filter