import Data.Char ( isAlpha, isLetter, toLower )
import qualified Data.Set as Set
import Data.List(nub, sort)

-- solved the problem with accumulator pattern
isPangram :: String -> Bool
isPangram text =
  length usedLetters == length alphabet
  where
    alphabet = ['a'..'z']
    lowText = map toLower text
    usedLetters = aux lowText []
    aux (x:xs) acc
      | (x `elem` acc) || x `notElem` alphabet = aux xs acc
      | otherwise                              = aux xs (x:acc)
    aux [] acc = acc

-- another solution with comprehensions and sets
isPangram' :: String -> Bool
isPangram' text =
  Set.fromList [toLower a | a <- text, isLetter a  ]
  == Set.fromList ['a'..'z']

-- yet another solution with library functions
isPangram'' :: String -> Bool
isPangram'' =
  (>= 26)
  . length
  . nub
  . filter isAlpha
  . map toLower
