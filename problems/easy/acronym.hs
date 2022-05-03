import Data.Char ( toUpper, isLower, isUpper )


replace :: Char -> Char
replace x = case x of
  '-' -> ' '
  '_' -> ' '
  c   ->  c

splitCamel :: [Char] -> [Char]
splitCamel "" = ""
splitCamel [x] = [x]
splitCamel (x:y:xs)
  | isLower x && isUpper y = x:' ':y:splitCamel xs
  | otherwise              = x:y:splitCamel xs


abbreviate :: String -> String
abbreviate xs = map (toUpper . head) $ words $ splitCamel $ map replace xs
