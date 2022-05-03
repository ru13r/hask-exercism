import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "Args:" ++ show args
  text <- readFile $ head args
  let wordCount = length $ words text
  putStrLn $ "Word count: " ++ show wordCount
