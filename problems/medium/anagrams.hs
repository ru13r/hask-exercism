module Anagram (anagramsFor) where
import Data.List ( sort )
import Data.Char ( toLower )

anagramsFor :: String -> [String] -> [String]
anagramsFor xs =
  filter(\a -> (sort . f) a == (sort . f) xs && f a /= f xs)
  where f = map toLower
