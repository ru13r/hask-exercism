module SecretHandshake (handshake) where
import Data.Bits (testBit)


strings :: [String]
strings = [ "wink"
          , "double blink"
          , "close your eyes"
          , "jump"
          ]

handshake :: Int -> [String]
handshake n = (if last flags then reverse else id) . filter (/="") $
  zipWith (\x y -> if x then y else "") flags strings
    where
      flags = [testBit] <*> [n] <*> [0..4]


-- community solution
handshake' n
  | xs <- [ "wink"            | testBit n 0 ]
       ++ [ "double blink"    | testBit n 1 ]
       ++ [ "close your eyes" | testBit n 2 ]
       ++ [ "jump"            | testBit n 3 ]
  = if testBit n 4
       then reverse xs
       else xs

-- another community solution
handshakeAcc :: [String] -> Int -> [String]
handshakeAcc acc i
    | n >= 16 = reverse (handshakeAcc acc (n-16))
    | n >= 8 = handshakeAcc ("jump":acc) (n-8)
    | n >= 4 = handshakeAcc ("close your eyes":acc) (n-4)
    | n >= 2 = handshakeAcc ("double blink":acc) (n-2)
    | n == 1 = handshakeAcc ("wink":acc) (n-1)
    | n == 0 = acc
    where n = i `mod` 32
