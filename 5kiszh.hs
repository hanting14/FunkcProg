import Control.Monad

-- Write an IO operation that reads a number, reads that many lines and prints
-- the number of lines that contained the character 'x'.

-- See the example below on how to read numbers:

example :: IO ()
example = do
  x <- readLn
  y <- readLn
  print (x + y)
  
CountX :: [String] -> Int
CountX [] = 0
CountX (a:as) = if (elem 'x' a) then (1 + (CountX as)) else (CountX as)

io :: IO ()
io = do
    n <- readLn
    lines <- replicateM n getLine
    print (CountX lines)
    
    
{-
  Some example evaluations:
  - Input:
      3
      something
      matrix
      axis
    Output:
      2

  - Input:
      4
      haskell
      exchange
      monad
      test
    Output:
      1
-}