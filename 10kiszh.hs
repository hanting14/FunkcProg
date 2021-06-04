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

evalParser :: Parser a -> String -> Maybe a
evalParser pa = (fst <$>) . runParser pa

execParser :: Parser a -> String -> Maybe String
execParser pa = (snd <$>) . runParser pa

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

-- Digit character with numeric value
digit :: Num a => Parser a
digit = (fromIntegral . digitToInt) <$> satisfy isDigit

-- Convert a list of digits into decimal a number
digitsToDecimalNumber :: Num a => [a] -> a
digitsToDecimalNumber = foldl (\acc curr -> acc * 10 + curr) 0

-- Non-negative integer number
number :: Num a => Parser a
number = digitsToDecimalNumber <$> some digit


{- Tasks -}

-- Task 1: Create an parser that extracts data from music filenames of the
--         following format:
--
-- (year)_(artist)-(title)\.(extension)
--   where
--     year          : [0-9]+
--     artist, title : [a-z]+
--     extension     : flac|mp3
--
-- Examples:
-- + evalParser p1 "2010_pendulum-encoder.flac" == Just (MkMusic 2010 "pendulum" "encoder" Lossless)
-- + evalParser p1 "2011_coldplay-paradise.mp3" == Just (MkMusic 2011 "coldplay" "paradise" Lossy)
-- + evalParser p1 "2012_muse-madness.flac" == Just (MkMusic 2012 "muse" "madness" Lossless)
-- + evalParser p1 "2013_adele-skyfall.mp3" == Just (MkMusic 2013 "adele" "skyfall" Lossy)
-- + evalParser p1 "???.jpg" == Nothing

data Quality = Lossless | Lossy deriving (Eq, Show)
data Music = MkMusic
  { year :: Int
  , artist :: String, title :: String
  , quality :: Quality
  } deriving (Eq, Show)

p1 :: Parser Music
p1 = do
	y <- foldl (\acc curr -> acc * 10 + curr) 0 <$> some digit
	--y <- many (satisfy isDigit)
	string "_"
	mus <- many (satisfy isLower)
	string "-"
	title <- many (satisfy isLower)
	string "."
	ext <-  (Lossless <$ string "flac") <|> (Lossy <$ string "mp3")
	return (MkMusic y mus title ext)
-- replicateM_ 3 (elemChar ['A'..'Z'])

-- Task 2: Create a parser that turns a string containing degrees, arcminutes
--         and arcseconds (with optional separators) to a floating point angle.
--
-- (degrees)°?(arcminutes)'?(arcseconds)('')?
--   where
--     degrees                : [0-9]{3}
--     arcminutes, arcseconds : [0-9]{2}
--
-- [1 degree = 60 arcminutes, 1 arcminute = 60 arcseconds]
--
-- Valid examples:
-- + evalParser p2 "202°04'27''" == Just 202.07417
-- + evalParser p2 "2020427" == Just 202.07417
-- + evalParser p2 "030°12'05''" == Just 30.20139
-- + evalParser p2 "0301205" == Just 30.20139
-- + evalParser p2 "foo" == Nothing

p2 :: Parser Float
p2 = do
	degrees <- foldl (\acc curr -> acc * 10 + curr) 0 <$> (replicateM 3 digit)
	optional (string "°")
	minutes <- foldl (\acc curr -> acc * 10 + curr) 0 <$> (replicateM 2 digit)
	optional (string "'")
	seconds <- foldl (\acc curr -> acc * 10 + curr) 0 <$> (replicateM 2 digit)
	optional (string "''")
	return (degrees + (minutes/60) + (seconds/3600))