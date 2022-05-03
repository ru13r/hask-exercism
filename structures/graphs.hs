hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath [] x y = x == y
hasPath xs a b
  | a == b    = True
  | otherwise =
    let xs' = [ (x, y) | (x,y) <- xs, x /= a] in
     or  [hasPath xs' y b | (x,y) <- xs, x == a]


l :: [(Int, Int)]
l = [(1,2), (2,3), (3,2), (4,3), (4,5)]
testPath :: [Bool]
testPath = map (uncurry $ hasPath l)
  [ (1, 2)
  , (2, 3)
  , (1, 3)
  , (4, 5)
  , (4, 2)
  , (4, 1)
  , (1, 4)
  ]
