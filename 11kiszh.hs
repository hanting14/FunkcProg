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

-- Tokenization:
char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

{- Tasks -}

-- Task 1: Create an parser that extracts information from a c++ variable:
--
-- Examples:
-- + evalParser variable "int age" == Just (MkVariable Int "age")
-- + evalParser variable "bool active" == Just (MkVariable Bool "active")
-- + evalParser variable "int * count" == Just (MkVariable (Pointer Int) "count")
-- + evalParser variable "bool * failed" == Just (MkVariable (Pointer Bool) "failed")
-- + evalParser variable "foo : int" == Nothing

data Type = Int | Bool | Pointer Type
            deriving (Eq, Show)
data Variable = MkVariable { varType :: Type, varName :: String }
                deriving (Eq, Show)

variable :: Parser Variable
variable = do
	typ <- ((Pointer Int) <$ string "int *") <|> ((Pointer Bool) <$ string "bool *") <|> (Int <$ string "int") <|> (Bool <$ string "bool")
	string " "
	name <- many (satisfy isLower) 
	return (MkVariable typ name)


-- Task 2: Create a parser that extracts information from a c++ function declaration:
--
-- Examples:
-- + evalParser function "int twice(int a);" == Just (MkFunction {funType = Int, funName = "twice", variables = [MkVariable {varType = Int, varName = "a"}]})
-- + evalParser function "bool negate(bool b);" == Just (MkFunction {funType = Bool, funName = "negate", variables = [MkVariable {varType = Bool, varName = "b"}]})
-- + evalParser function "int count(int * match, bool all);" == Just (MkFunction Int "count" [MkVariable (Pointer Int) "match", MkVariable Bool "all"])
-- + evalParser function "bool authenticate(int token, bool secure);" == Just (MkFunction Bool "authenticate" [MkVariable Int "token", MkVariable Bool "secure"])
-- + evalParser function "foo -> bar(asdf qwerty);" == Nothing

data Function =
  MkFunction { funType :: Type, funName :: String, variables :: [Variable] }
  deriving (Eq, Show)

function :: Parser Function
function = do
	typ <- ((Pointer Int) <$ string "int *") <|> ((Pointer Bool) <$ string "bool *" <|> (Int <$ string "int") <|> (Bool <$ string "bool"))
	string " "
	funname <- many (satisfy isLower)
	string "("
	varies <- variable 
	varies2 <- many ((string ", ") >> variable)
	string ");"
	return (MkFunction typ funname (varies:varies2))