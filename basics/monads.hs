import           Control.Monad              (filterM, join)
import           Control.Monad.Trans.Writer


f :: Integer -> Integer
f x = x + 1


g :: Integer -> Integer
g x = 2 * x


isZero :: Integer -> Bool
isZero x
  | x == 0    = True
  | otherwise = False


divide :: Float -> Float -> Maybe Float
divide x y
  | y /= 0    = Just (x / y)
  | otherwise = Nothing


madd :: (Monad m, Num b) => m b -> m b -> m b
madd mx my =
  mx >>= \x ->
  my >>= \y ->
  return $ x + y


-- do
-- x <- m
-- m >>= (\x -> ...)


madd' :: (Monad m, Num b) => m b -> m b -> m b
madd' mx my = do
  x <- mx
  y <- my
  return(x +y)


readnum :: IO Int
readnum = do
  putStrLn "Input 2 numbers:"
  x <- readLn
  y <- readLn
  putStrLn "The sum:"
  return(x+y)

-- >>> rednum

readnum' :: IO Int
readnum' =
  putStrLn "Input 2 numbers:" >>
  readLn >>= \x ->
  readLn >>= \y ->
  putStrLn "The sum:" >>
  return(x + y)


flatten :: Monad m => m (m a) -> m a
flatten = join

-- Writer example
gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

-- liftM
liftM' :: (Monad m) => (a -> b) -> m a -> m b
liftM' f m = do
    f <$> m

-- ap is the same as <*> only it has the monad as the constraint
ap' :: (Monad m) => m (a -> b) -> m a -> m b
ap' mf m = do
    f <- mf
    f <$> m

-- filterM
keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4 = do
        tell ["Keeping " ++ show x]
        return True
    | otherwise = do
        tell [show x ++ " is too large, throwing it away"]
        return False

mfilter = filterM keepSmall [1,3,5,6,3]

wresults :: [Int]
wresults = fst $ runWriter mfilter

wlog :: IO ()
wlog = mapM_ putStrLn $ snd $ runWriter mfilter

