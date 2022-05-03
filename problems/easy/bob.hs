module Bob (responseFor) where
import Data.Char ( isDigit, toUpper, isAlpha, isSpace )

-- Bob is a lackadaisical teenager. In conversation, his responses are very limited.
-- Bob answers 'Sure.' if you ask him a question, such as "How are you?".
-- He answers 'Whoa, chill out!' if you YELL AT HIM (in all capitals).
-- He answers 'Calm down, I know what I'm doing!' if you yell a question at him.
-- He says 'Fine. Be that way!' if you address him without actually saying anything.

-- He answers 'Whatever.' to anything else.
isQuestion :: [Char] -> Bool
isQuestion xs = last xs == '?'
isYell :: [Char] -> Bool
isYell xs = (xs == map toUpper xs) && any isAlpha xs

responseFor :: String -> String
responseFor "" = "Fine. Be that way!"
responseFor ys
  | xs == ""                         = "Fine. Be that way!"
  | isQuestion xs && isYell xs       = "Calm down, I know what I'm doing!"
  | isQuestion xs && not (isYell xs) =  "Sure."
  | not (isQuestion xs) && isYell xs = "Whoa, chill out!"
  | otherwise                        = "Whatever."
  where xs = filter (not . isSpace) ys
