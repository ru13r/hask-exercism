
toInt x = read x :: Int

armstrong n = sum (map (^k) ns) == n where
  k  = length $ show n
  ns =  map (toInt . (:"")) $ show n
