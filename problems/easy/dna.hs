-- G -> C
-- C -> G
-- T -> A
-- A -> U

module DNA (toRNA) where

append acc l = acc >>= (\a -> Right(a ++ l))
toRNA :: String -> Either Char String
toRNA = foldl f (Right "")
 where
   f acc x
     | x == 'G'  = append acc "C"
     | x == 'C'  = append acc "G"
     | x == 'T'  = append acc "A"
     | x == 'A'  = append acc "U"
     | otherwise = Left x


-- Community solution with traverse
toRNA1 :: String -> Either Char String
toRNA1 = traverse fromDNA
  where
    fromDNA :: Char -> Either Char Char
    fromDNA 'G' = pure 'C'
    fromDNA 'C' = pure 'G'
    fromDNA 'T' = pure 'A'
    fromDNA 'A' = pure 'U'
    fromDNA c = Left c

-- Community solution with mapM
-- Map each element of a structure to a monadic action,
-- evaluate these actions from left to right,
-- and collect the results.

toRNA2 :: String -> Either Char String
toRNA2 = mapM f
    where
        f 'A' = Right 'U'
        f 'C' = Right 'G'
        f 'G' = Right 'C'
        f 'T' = Right 'A'
        f x   = Left x

-- mapM is equivalent to sequence $ map
toRNA3 :: String -> Either Char String
toRNA3 xs = sequence $ map f xs
    where
      f 'A' = Right 'U'
      f 'C' = Right 'G'
      f 'G' = Right 'C'
      f 'T' = Right 'A'
      f x   = Left x

-- Solution with applicative functor
toRNA4 :: String -> Either Char String
toRNA4 [] = Right ""
toRNA4 (h:q)
  | h == 'G' = (++) <$> Right "C" <*> toRNA q
  | h == 'C' = (++) <$> Right "G" <*> toRNA q
  | h == 'T' = (++) <$> Right "A" <*> toRNA q
  | h == 'A' = (++) <$> Right "U" <*> toRNA q
  | otherwise  = Left  h
