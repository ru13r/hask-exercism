module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, fromList, adjust)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

inits :: Either String (Map Nucleotide Int)
inits = Right (fromList [(A, 0),(C,0),(G,0),(T,0)])

key :: Char -> Either String Nucleotide
key x =case x of
  'A' -> Right A
  'C' -> Right C
  'G' -> Right G
  'T' -> Right T
  _   -> Left (x:" is not a nucleotide")

mapp ::  Either String (Map Nucleotide Int) -> Either String Nucleotide
  ->  Either String (Map Nucleotide Int)
mapp (Left x) _ = Left x
mapp _ (Left x) = Left x
mapp  acc (Right k)  = acc >>=  Right . adjust (+1) k

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts "" = inits
nucleotideCounts xs = foldl mapp inits $ map key xs
