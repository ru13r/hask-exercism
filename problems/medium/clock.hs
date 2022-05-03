module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock { getHour :: Int
                   , getMinute :: Int
                   } deriving Eq

fromHourMin :: Int -> Int -> Clock
fromHourMin h m = Clock h1 m1 where
     h1 = (h + h') `mod` 24
     m1 = m `mod` 60
     h' = m `div` 60

toString :: Clock -> String
toString (Clock h m) = pad h ++ ":" ++ pad m
  where
    pad x = if x > 9 then show x else "0" ++ show x

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock h m) =
  Clock h1 m1 where
    h1 = (h + hour + h') `mod` 24
    m1 = (m + min) `mod` 60
    h' = (m + min) `div` 60
