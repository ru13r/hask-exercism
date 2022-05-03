module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs


(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x:xs) ys = x : xs ++ ys


foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = x `f` foldr f z xs

length :: [a] -> Int
length xs = foldr (\_ z -> 1+z) 0 xs

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z [] = z
foldl' f z (x:xs) = let z' = z `f` x in seq z' $ foldl' f z' xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

reverse :: [a] -> [a]
reverse = foldr (\x z -> z ++ [x]) []
