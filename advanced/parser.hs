module Main where
import           Control.Applicative
import           Data.Char

data JsonValue = JsonNull
               | JsonBool Bool
               | JsonNumber Integer -- no support for floats yet
               | JsonString String
               | JsonArray [JsonValue]
               | JsonObject [(String, JsonValue)]
               deriving (Show, Eq)

-- NOTE: No proper error reporting
newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (input', x) <- p input
    Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  Parser p1 <*> Parser p2 = Parser $ \input -> do
    (input', f)  <- p1 input
    (input'', x) <- p2 input'
    Just (input'', f x)

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser p1 <|> Parser p2 = Parser $ \input ->
    p1 input <|> p2 input


jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false") where
  f "true"  = JsonBool True
  f "false" = JsonBool False
  f _       = undefined      -- this will never happen

jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNull (spanP isDigit) where
  f ds = JsonNumber $ read ds

jsonString :: Parser JsonValue
jsonString = JsonString <$> (charP '"' *> stringLiteral <* charP '"')

jsonObject :: Parser JsonValue
jsonObject = JsonObject
             <$> (charP '{'
                  *> ws
                  *> sepBy ( ws *> charP ',' <* ws) pair
                  <* ws
                  <* charP '}')
                  where
                    pair = (\key _ value -> (key, value))
                      <$> stringLiteral
                      <*> (ws *> charP ':' <* ws)
                      <*> jsonValue

ws :: Parser String
ws = spanP isSpace


sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:)


  <$> element
  <*> many (sep *> element)
  <|> pure []


jsonArray :: Parser JsonValue
jsonArray = JsonArray <$>
  (charP '['
   *> ws *>
   elements
   <* ws <*
   charP ']')
  where
    elements = sepBy sep jsonValue
    sep = ws *> charP ',' <* ws


stringLiteral :: Parser String
stringLiteral = charP '"' *>  spanP (/= '"') <* charP '"'

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
  let (token, rest) = span f input
  in Just (rest, token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \input -> do
  (input', xs) <- p input
  if null xs
    then Nothing
    else Just (input', xs)

charP :: Char -> Parser Char
charP x = Parser f where
  f (y:ys)
    | y == x    = Just (ys, x)
    | otherwise = Nothing
  f []          = Nothing

stringP :: String -> Parser String
stringP = traverse charP

jsonValue :: Parser JsonValue
jsonValue =
      jsonNull
  <|> jsonBool
  <|> jsonNumber
  <|> jsonString
  <|> jsonObject

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile filename parser = do
  input <- readFile filename
  return (snd <$> runParser parser input)

main :: IO()
main = undefined
