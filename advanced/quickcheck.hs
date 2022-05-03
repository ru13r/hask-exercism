import           Test.QuickCheck

prop :: (Eq a, Num a) => a -> a -> Bool
prop a b = (a+b) == (b+a)

prop1 :: [a] -> Bool
prop1 xs = length (tail xs) == (length xs -1)
