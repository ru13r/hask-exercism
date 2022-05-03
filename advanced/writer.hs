data Writer w a = Writer w a  deriving (Show)
runWriter :: Writer w a -> w
runWriter (Writer s _) = s

instance Functor (Writer w) where
  fmap f (Writer w a) = Writer w b where b = f a
instance Monoid w => Applicative (Writer w) where
  pure x = Writer mempty x
  (Writer w a) <*> (Writer w' b) = Writer (w `mappend` w') (a b)

instance Monoid w => Monad (Writer w) where
  (Writer s a) >>= f = let (Writer s' b) = f a in
    Writer (s `mappend` s') b

wdiv ::  Integer -> Integer -> Writer String Integer
wdiv x y = Writer (
  show x ++ "\t| "
  ++ show y ++ "\t=\t" ++ show z ++ "\n") z where z = x `div` y



-- ldiv ::  Integer -> Integer -> Writer [Integer] Integer
-- ldiv x y = Writer [y] z
--   where z = x `div` y

factors :: Integer -> Writer String Integer
factors 1 = Writer "" 1
factors n = do
    let k = head [r | r <- [2..n], n `mod` r == 0]
    m <- n `wdiv` k
    factors m
-- >>>  runWriter $ factors 11
-- "11\t| 11\t=\t1\n"


main :: IO ()
main = do
  putStrLn "Please, provide the natural number"
  x <- readLn
  putStrLn . runWriter $ factors x
