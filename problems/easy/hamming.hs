distance :: String -> String -> Maybe Int
distance "" "" = Just 0
distance "" _ = Nothing
distance _ "" = Nothing
distance xs ys
  | length xs == length ys =
      Just $ length . filter (==False) $ zipWith (==) xs ys
  | otherwise              = Nothing

-- Nice use of counter with monads
distance' :: String -> String -> Maybe Int
distance' [] [] = Just 0
distance' (x:xs) (y:ys)
  | x /= y    = (+1) <$> distance' xs ys
  | otherwise = distance' xs ys
distance' _ _ = Nothing
