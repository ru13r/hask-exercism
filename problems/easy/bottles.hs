import Data.List ( intercalate )


bob :: (Show a, Num a, Ord a) => a -> [Char]
bob x = y ++ " bottle" ++ s  ++ " of beer"
  where
    y = if x >  0 then show x else "no more"
    s = if x /= 1 then "s" else ""


otw = " on the wall"
tod x = "Take "++ one ++ " down and pass it around" where
  one = if x > 1 then "one" else "it"

bottles :: (Show a, Num a, Ord a) => a -> [Char]
bottles x =
  bob x ++ otw ++  ", " ++ bob x ++ ".\n" ++
  tod x++ ", " ++  bob (x-1) ++ otw ++ "."

song :: String
song = intercalate "\n\n" $  map bottles [99, 98..1] ++ [lst]
  where
    lst = "No more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall.\n"
