sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ filter (\x -> 0 `elem` (fs <*> [x])) [1..(limit - 1)]
  where fs = flip mod <$> filter (/= 0) factors
