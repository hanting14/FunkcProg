{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

import Data.Maybe
import Data.Char
import Control.Applicative
import Control.Monad

{- Parser -}

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  -- No input is consumed
  return :: a -> Parser a
  return a = Parser $ \s -> Just (a, s)

  -- Executing parsers sequentially
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing      -> Nothing
      Just (a, s') -> runParser (g a) s'

instance Alternative Parser where
  -- Parser that fails instantly
  empty :: Parser a
  empty = Parser $ const Nothing

  -- Try the first parser, in case of failure try the second one
  -- (p1 <|> p2 is practically equivalent to the `p1|p2` RegEx)
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser f) (Parser g) =
    Parser $ \s -> case f s of
      Nothing      -> g s
      Just (a, s') -> Just (a, s')


-- Basic parsers

-- "end of file" - succeeds only on empty input ($ in RegEx)
eof :: Parser ()
eof = Parser $ \case
  [] -> Just ((), [])
  _  -> Nothing

-- Read a character if it satisfies a certain criteria
-- (And the input is not empty.)
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \case
  c:cs | f c -> Just (c, cs)
  _          -> Nothing

-- Read a specific character
char :: Char -> Parser ()
char c = () <$ satisfy (== c)

-- Read any character (. in RegEx)
anyChar :: Parser Char
anyChar = satisfy (const True)

-- Read a specific string of characters
string :: String -> Parser ()
string = mapM_ char


-- From Control.Applicative:

-- Zero or more repetitions (* in RegEx)
-- many :: Parser a -> Parser [a]

-- One or more repetitions (+ in RegEx)
-- some :: Parser a -> Parser [a]

-- Zero or one repetition (? in RegEx)
-- optional :: Parser a -> Parser (Maybe a)

many_ :: Parser a -> Parser ()
many_ p = () <$ many p

some_ :: Parser a -> Parser ()
some_ p = () <$ some p

optional_ :: Parser a -> Parser ()
optional_ p = () <$ optional p

-- Element of a list ([...] in RegEx)
elemChar :: [Char] -> Parser Char
elemChar chars = satisfy (`elem` chars)

-- RegEx match checker
match :: Parser a -> String -> Bool
match p s = isJust (runParser p s)


{- Example -}

-- Parser, that matched hexadecimal color literals of the followig format:
--
-- 0x[0-9A-F]{6}$
--
-- Valid examples:
-- + "0x000000"
-- + "0xFA55B8"
--
-- Invalid examples:
-- + "1337AB"
-- + "0xAA1Q34"

p0 :: Parser ()
p0 = string "0x" >> replicateM_ 6 (elemChar (['0'..'9'] ++ ['A'..'F'])) >> eof


{- Tasks -}

-- Task 1: Create an parser that matches Hungarian license plate numbers.
--
-- [A-Z]{3}-\d{3}$
--
-- Valid examples:
-- + "ABC-123"
-- + "IRF-764"
-- + "LGM-859"
--
-- Invalid examples:
-- + "ABCD-1234"
-- + "ABC-123asdf"
-- + "ABC123"
-- + "123-ABC"

p1 :: Parser ()
p1 = replicateM_ 3 (elemChar ['A'..'Z']) >> string "-" >> replicateM_ 3 (elemChar ['0'..'9']) >> eof


-- Task 2: Create a parser that matches Hungarian street names and house numbers.
--
-- ([A-Z][a-z]+ )+(utca|út) \d+(/[A-Z])?\.
--
-- Valid examples:
-- + "Kertész utca 4."
-- + "Szabadság út 128/B."
-- + "Arany János utca 8."
-- + "Neumann János út 53/A. 2. emelet 5. ajtó"
--
-- Invalid examples:
-- + "Szabadság út B/128."
-- + "Liget köz 8."
-- + "Kertész utca 4"
-- + "PetőfiSándor utca 7/D."

p2 :: Parser ()
p2 =  do
	some_ ((elemChar ['A'..'Z']) >> (some_ (elemChar (['a'..'z'] ++ ['é','í','á','ő','ó','ű','ú'])) >> (string " ")))
	(string "utca " <|> string "út ")
	some_ (elemChar ['0'..'9'])
	optional_ ((string "/") >> (some_ (elemChar ['A'..'Z'])))
	string "."