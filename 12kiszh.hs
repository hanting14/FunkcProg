{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

import Data.Maybe
import Data.Char
import Control.Applicative
import Control.Monad
import Debug.Trace


{- Parser -}

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

evalParser :: Parser a -> String -> Maybe a
evalParser pa = (fst <$>) . runParser pa

execParser :: Parser a -> String -> Maybe String
execParser pa = (snd <$>) . runParser pa

-- (State String + Maybe) monad
-- Parser a : function, which can modify a String state and can potentially fail
--   Nothing     : parse error
--   Just (a, s) : successful parsing with result and remaining input

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  -- No input is consumed
  return :: a -> Parser a
  return a = Parser $ \s -> Just (a, s)
  -- runParser (return 10) "asdf" == Just (10,"asdf")

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


debug :: String -> Parser ()
debug msg = Parser $
  \s -> trace ("{- " ++ msg ++ " # input: " ++ show s ++ " -}") (Just ((), s))

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

-- Read one or more elements with separators between them
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep >> pa)

-- Read zero or more elements with separators between them
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

-- Parser version of 'foldl'
chainl :: (b -> a -> b) -> Parser b -> Parser a -> Parser b
chainl f pb pa = do {b <- pb; go b} where
  go b = (do {a <- pa; go (f b a)}) <|> pure b

-- Element of a list ([...] in RegEx)
elemChar :: [Char] -> Parser Char
elemChar chars = satisfy (`elem` chars)

-- Whitespace, such as space, tab, newline (\s in RegEx)
whitespace :: Parser Char
whitespace = elemChar [' ', '\n', '\t']

-- A continous section of zero or more whitespace
ws :: Parser ()
ws = () <$ many whitespace

-- Digit character with numeric value
digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

-- Convert a list of digits into a number
digitsToNumber :: Int -> [Int] -> Int
digitsToNumber base = foldl ((+) . (*base)) 0

-- Non-negative integer number
number :: Parser Int
number = digitsToNumber 10 <$> some digit

-- Tokenization:
char' :: Char -> Parser ()
char' c = ws *> char c <* ws

char'' :: Char -> Parser ()
char'' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

-- Auxiliary operator parser functions
infixLeft :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
infixLeft f pa psep = foldl1 f <$> sepBy1 pa psep

infixRight :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
infixRight f pa psep = foldr1 f <$> sepBy1 pa psep

infixNonAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
infixNonAssoc f pa psep = do
  es <- sepBy1 pa psep
  case es of
    [e]      -> pure e
    [e1, e2] -> pure $ f e1 e2
    _        -> empty

prefixAssoc :: (a -> a) -> Parser op -> Parser a -> Parser a
prefixAssoc f pop pa = (pop *> (f <$> prefixAssoc f pop pa)) <|> pa

prefixNonAssoc :: (a -> a) -> Parser op -> Parser a -> Parser a
prefixNonAssoc f pop pa = (pop *> (f <$> pa)) <|> pa


{- Task -}

data Exp
  = Lit Int
  | Sub Exp Exp
  | Mul Exp Exp
  | Gt Exp Exp
  deriving (Eq, Show)


-- Task 1: Create a parser that builds syntax trees out of expressions
--         containing literals, substraction, multiplication and "greater than"
--         operators.
--
--         Their precedence is in the following order (from strong to weak):
--         (*) - right associative
--         (-) - left associative
--         (>) - non associative
--
-- Examples:
-- + parse "9 - 5" == Just (Sub (Lit 9) (Lit 5))
-- + parse "15 - 7 - 3" == Just (Sub (Sub (Lit 15) (Lit 7)) (Lit 3))
-- + parse "3 * 4" == Just (Mul (Lit 3) (Lit 4))
-- + parse " 7 - 2  * 2" == Just (Sub (Lit 7) (Mul (Lit 2) (Lit 2)))
-- + parse "(7 - 2) * 2" == Just (Mul (Sub (Lit 7) (Lit 2)) (Lit 2))
-- + parse "8 > 6" == Just (Gt (Lit 8) (Lit 6))
-- + parse "1 > 6" == Just (Gt (Lit 1) (Lit 6))
-- + parse "(12 - 4) * 3 > 12 - 4 * 3" == Just (Gt (Mul (Sub (Lit 12) (Lit 4)) (Lit 3)) (Sub (Lit 12) (Mul (Lit 4) (Lit 3))))
-- + parse "10 * (5 - 3) > 10 * 5 - 3" == Just (Gt (Mul (Lit 10) (Sub (Lit 5) (Lit 3))) (Sub (Mul (Lit 10) (Lit 5)) (Lit 3)))
-- + parse "1 - 2 qwerty" == Nothing

pLit :: Parser Exp
pLit = do
	x <- number
	pure (Lit x)

pPar :: Parser Exp
pPar = char'' '(' *> pGt <* char'' ')'

pAtom :: Parser Exp
pAtom = pPar <|> pLit

pMul :: Parser Exp
pMul = infixRight Mul pAtom (char' '*')

pSub :: Parser Exp
pSub = infixLeft Sub pMul (char' '-')

pGt :: Parser Exp
pGt = infixNonAssoc Gt pSub (char' '>')

pExp :: Parser Exp
pExp = topLevel pGt

parse :: String -> Maybe Exp
parse = evalParser pExp


-- Task 2: Create an evaluator that produces Int values from such syntax trees!
--         The "greater than" operator should be implemented using "C-style"
--         booleans, meaning that 0 should be returned for False and 1 for True.
--
-- Examples:
-- + eval "9 - 5" == 4
-- + eval "15 - 7 - 3" == 5
-- + eval "3 * 4" == 12
-- + eval " 7 - 2  * 2" == 3
-- + eval "(7 - 2) * 2" == 10
-- + eval "8 > 6" == 1
-- + eval "1 > 6" == 0
-- + eval "(12 - 4) * 3 > 12 - 4 * 3" == 1
-- + eval "10 * (5 - 3) > 10 * 5 - 3" == 0

evalExp :: Exp -> Int
evalExp (Lit i) = i
evalExp (Sub e1 e2) = evalExp e1 - evalExp e2
evalExp (Mul e1 e2) = evalExp e1 * evalExp e2
evalExp (Gt e1 e2) = if evalExp e1 > evalExp e2 then 1 else 0

eval :: String -> Int
eval = evalExp . fromJust . parse