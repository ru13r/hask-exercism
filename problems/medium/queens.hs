
module Queens (boardString, canAttack) where
import Data.List

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines [ unwords [ [chr x y] | y <- [0..7] ] | x <- [0..7] ] where
    chr x y
      | Just (x, y) == white   = 'W'
      | Just (x, y) == black   = 'B'
      | otherwise              = '_'

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack queenA@(xa, ya) queenB@(xb, yb)
  | xa == xb || ya == yb = True
  | xa - ya == xb - yb   = True
  | xa + ya == xb + yb   = True
  | otherwise            = False
