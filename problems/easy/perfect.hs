module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

factors :: Int -> [Int]
factors n
  | n <= 0  = []
  | otherwise = [x | x <- [1..n], x  <= n `div` 2, n `mod` x == 0 ]

aliquot :: Int -> Int
aliquot n = sum $ factors n

classify :: Int -> Maybe Classification
classify n
  | n <= 0         = Nothing
  | aliquot n == n = Just Perfect
  | aliquot n <  n = Just Deficient
  | otherwise      = Just Abundant
