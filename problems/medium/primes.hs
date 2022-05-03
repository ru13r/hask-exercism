nth :: Int -> Maybe Integer
nth n
  | n >= 1    = Just $ last $ take n $ primes (2:[3,5..])
  | otherwise = Nothing
  where
   primes :: [Integer] -> [Integer]
   primes [] = []
   primes (x:xs) = x : primes (filter ((/= 0) . (`mod` x)) xs)
