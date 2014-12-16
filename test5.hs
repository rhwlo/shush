-- tokenization

-- tokenize on the following:
--   - p`...` where ... is anything but ` (not counting an escaped \`)
--   - e`...` where ... matches above
--   - `...`  where ... matches above
--   - ... where ... is anything but a space character

import Data.Char

matchToken :: String -> ((Tokenized String), String, (Char -> Bool))
matchToken ('p':'`':xs) = (Token (Pattern, ""), xs, (== '`'))
matchToken ('e':'`':xs) = (Token (External, ""), xs, (== '`'))
matchToken ('`':xs) = (Token (Simple, ""), xs, (== '`'))
matchToken xs = (Token (Simple, ""), xs, (isSpace))

data TokenType = Simple | Pattern | External deriving (Eq, Show)

data Tokenized a = Token (TokenType, a) | NotToken deriving (Eq, Show)

instance Functor Tokenized where
  fmap f (Token (t, x)) = Token (t, (f x))
  fmap _ NotToken = NotToken

tokenAppend :: Tokenized [a] -> [a] -> Tokenized [a]
tokenAppend (Token (t, xs)) s = Token (t, (xs ++ s))
tokenAppend (NotToken) _ = NotToken

tLast :: Tokenized [a] -> a
tLast (Token (_, xs)) = last xs

tInit :: Tokenized [a] -> [a]
tInit (Token (_, xs)) = init xs

tokenLength :: Tokenized [a] -> Int
tokenLength (Token (_, xs)) = length xs

dummyToken :: String -> Tokenized String
dummyToken _ = NotToken

-- tokenizeOn :: charMatch -> stringInProgress -> progressSoFar -> stringToTokenize -> result
-- if charMatch fits, and the previous characters don't indicate that it's escaped,
--   append stringInProgress to progressSoFar and continue iterating on stringToTokenize
-- if charMatch does not fit, append current Char to stringInProgress and continue
tokenizeOn :: (Char -> Bool) -> Tokenized String -> [Tokenized String] -> String -> [Tokenized String]
tokenizeOn _ tokenInProgress progressSoFar "" = progressSoFar ++ [tokenInProgress]
tokenizeOn _ NotToken progressSoFar stringToTokenize
  = let (startToken, stringToTokenize', charMatch) = matchToken stringToTokenize in
    tokenizeOn charMatch startToken progressSoFar stringToTokenize'
tokenizeOn charMatch tokenInProgress progressSoFar (x:stringToTokenize)
  | charMatch x && (tokenLength tokenInProgress > 0 && lastChar /= '\\' || (tokenLength tokenInProgress > 1 && penultChar == '\\'))
    = tokenizeOn charMatch NotToken (progressSoFar ++ [tokenInProgress]) (lstrip stringToTokenize)
  | otherwise
    = tokenizeOn charMatch (tokenAppend tokenInProgress [x]) progressSoFar stringToTokenize
  where lastChar = tLast tokenInProgress
        penultChar = last (tInit tokenInProgress)


tokenize :: String -> [Tokenized String]
tokenize s = tokenizeOn isSpace NotToken [] s

lstrip = dropWhile isSpace

main :: IO ()
main = print (tokenize "singleword `literal phrase` p`pattern` e`external`")