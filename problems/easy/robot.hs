module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

type Coords = (Integer, Integer)

data Robot = Robot { bearing :: Bearing
                   , coordinates :: Coords
                   } deriving (Show)

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

int :: Bearing -> Int
int North = 0
int East = 1
int South = 2
int West = 3

shift :: Bearing -> Coords -> Coords
shift b c = case b of
   North -> (fst c    , snd c + 1)
   East  -> (fst c + 1, snd c    )
   South -> (fst c    , snd c - 1)
   West  -> (fst c - 1, snd c    )

unint :: Int -> Bearing
unint 0 = North
unint 1 = East
unint 2 = South
unint 3 = West
unint n = unint $ n `mod` 4

rotateRight :: Robot -> Robot
rotateRight r = mkRobot b1 (coordinates r)
  where b1 = unint . (+1) . int $ bearing r

rotateLeft :: Robot -> Robot
rotateLeft r = mkRobot b1 (coordinates r)
  where b1 = unint . (+(-1)) . int $ bearing r


advance :: Robot -> Robot
advance r = mkRobot bg (shift bg cs)
                where
                  bg = bearing r
                  cs = coordinates r

cmd :: Char -> Robot -> Robot
cmd 'R' = rotateRight
cmd 'L' = rotateLeft
cmd 'A' = advance
cmd _   = id

move :: Robot -> String -> Robot
move r cs = foldl (\a r -> r a) r (cmd <$> cs)
