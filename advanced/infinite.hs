
ones :: [Integer]
ones = 1 : ones

nat :: [Integer]
nat = asc 1 where asc n = 1 : asc  (n + 1)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
