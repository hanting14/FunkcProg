import Prelude hiding (read)
import Control.Monad
import Control.Monad.State

whileM :: Monad m => m Bool -> m a -> m [a]
whileM mb ma = do
  b <- mb
  if b then do
      a <- ma
      rest <- whileM mb ma
      pure (a:rest)
    else pure []


{- Tape -}

type Tape a = ([a], a, [a])

read :: State (Tape a) a
read = do
  (left, center, right) <- get
  return center

write :: a -> State (Tape a) ()
write a = do
  (left, center, right) <- get
  put (left, a, right)

left :: State (Tape a) ()
left = do
  (left, center, right) <- get
  put (tail left, head left, center:right)

right :: State (Tape a) ()
right = do
  (left, center, right) <- get
  put (center:left, head right, tail right)


-- Tape Char operations

printTape :: Tape Char -> String
printTape (left, center, right) = reverse left ++ [center] ++ right

-- printTape exampleTape == "this is a test"
--                                   ^
exampleTape :: Tape Char
exampleTape = (" si siht", 'a', " test")

-- printTape $ execState example exampleTape == "that is a test"
-- evalState example exampleTape == 'i'
example :: State (Tape Char) Char
example = do
  replicateM 6 left
  write 'a'
  right
  write 't'
  replicateM 2 right
  read


{- Tasks -}


-- Constraint: Direct State operations are not allowed,
--             use only the monadic Tape interface!
--             [read, write, left, right]

-- Task 1: Create an operation that overwrites the tape from the current
--         position with characters from the string given as parameter.
--
-- Example: _exa___mple_ --> writeWord "test" --> _exa_testle_
--               ^                                         ^


writeWord :: String -> State (Tape Char) ()
writeWord [] = pure ()
writeWord (x:xs) = do
	write x
	right
	writeWord xs

-- Task 2: Create an operation that reads characters from the tape until it
--         encounters a space character, after which it returns the concatenated
--         result as a string.
--
-- Example: _exam__ple_ --> readWord --> ("xam", _exam__ple_)
--            ^                                       ^

readWord :: State (Tape Char) String
readWord = do
	x <- read
	if x == ' ' then (pure x) else ((pure x):readWord)


-- Definitions for test cases:

emptyTape :: Tape Char
emptyTape = ("       ", ' ', "       ")

rM :: Applicative m => Int -> m a -> m [a]
rM = replicateM

evS :: State s a -> s -> a
evS = evalState

exS :: State s a -> s -> s
exS = execState

pT :: Tape Char -> String
pT = printTape

eT :: Tape Char
eT = emptyTape

wW :: String -> State (Tape Char) ()
wW = writeWord

rW :: State (Tape Char) String
rW = readWord

{-
  Some example evaluations:
  - pT (exS (do {wW "example"}) eT) == "       example "
  - pT (exS (do {rM 5 left; wW "test"; rM 2 right; wW "asdf"}) eT) == "  test  asdf   "
  - pT (exS (do {rM 3 left; wW "hamburger"; rM 12 left; wW "cheese"}) eT) == " cheeseburger  "

  - evS (do {rW}) ("  ", 'q', "werty uiop ") == "qwerty"
  - evS (do {rM 8 right; rW}) ("  ", 'q', "werty uiop ") == "iop"
  - evS (do {rM 3 left; wW "foobar"; rM 3 left; rW}) eT == "bar"
-}