module Scrabble (scoreLetter, scoreWord) where
import Data.Char

scoreLetter :: Char -> Integer
scoreLetter letter = (f . toUpper) letter where
  f letter
   | letter `elem` "AEIOULNRST" = 1
   | letter `elem` "DG"         = 2
   | letter `elem` "BCMP"       = 3
   | letter `elem` "FHVWY"      = 4
   | letter `elem` "K"          = 5
   | letter `elem` "JX"         = 8
   | letter `elem` "QZ"         = 10
   | otherwise                  = 0

scoreWord :: String -> Integer
scoreWord = foldr (\x acc -> acc + scoreLetter x) 0
