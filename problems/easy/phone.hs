module Phone (number) where
import Data.Char ( isNumber )

proc10 num@(x1:x2:x3:x4:xs)
  | x1 >= '2' && x4 >= '2' = Just num
  | otherwise              = Nothing

number :: String -> Maybe String
number xs = f $ filter isNumber xs
  where
    f (x:xs) = case length (x:xs) of
      11 -> if x == '1' then proc10 xs else Nothing
      10 -> proc10 (x:xs)
      _  -> Nothing
