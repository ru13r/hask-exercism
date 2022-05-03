module ETL (transform) where

import Data.Map (Map, lookup, insert, empty, fold, assocs, union, fromList, toList)
import Data.Char


transform' :: Foldable t => (a, t Char) -> Map Char a
transform' (k, vs) = foldl (\acc x -> insert (toLower x) k acc) empty vs


transform :: Map a String -> Map Char a
transform legacyData = foldr (union . transform') empty $ assocs legacyData

-- community solution with comprehensions
transform2 :: Map a String -> Map Char a
transform2 legacyData =
  fromList [ (toLower l,s) | (s,ls) <- toList legacyData, l <- ls ]
