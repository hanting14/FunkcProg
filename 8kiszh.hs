{-# LANGUAGE DeriveTraversable #-}

import Data.List
import Control.Monad.State

quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser = filter (< p) xs
        greater = filter (>= p) xs

data Tree a
  = L a
  | N (Tree a) (Tree a)
  deriving (Functor, Foldable, Traversable)

-- Task 1: An ant is walking on the number line. It can either take three steps
--         right or five steps left given a starting number and a number of
--         steps, list the numbers that it can end up at after the last step.

antStepsFrom :: Int -> Int -> [Int]
antStepsFrom start steps = do
  x <- [-5, 3]
  y <- [-5, 3]
  mult <- [0..steps]
  mult2 <- [0..steps]
  guard ((mult + mult2) == steps)
  return (start + (x*mult) + (y*mult2))

-- Task 2: Count the number of even and odd elements in a traversable structure.
--         The final pair should contain the results in the (#even, #odd) order.

countEvenOdd :: Traversable t => t Int -> (Int, Int)
countEvenOdd t = execState (traverse go t) (0, 0) where
  go :: Int -> State (Int, Int) ()
  go i = do
    (x,y) <- get
    if (even i) then do
		put (x+1, y)
		pure ()
    else do
		put (x, y+1)
		pure ()


{-
  Some example evaluations:
  - (quicksort . nub) (antStepsFrom (-1) 0) == [-1]
  - (quicksort . nub) (antStepsFrom 0 2) == [-10, -2, 6]
  - (quicksort . nub) (antStepsFrom 3 5) == [-22,-14,-6,2,10,18]

  - countEvenOdd [] == (0, 0)
  - countEvenOdd (N (L 5) (L (-10))) == (1, 1)
  - countEvenOdd [4, 8, 20, 5, 16, 1, 0] == (5, 2)
  - countEvenOdd (N (N (L 5) (N (L 7) (L (-12)))) (N (L 3) (L (-8)))) == (2, 3)
  - countEvenOdd (N (N (L (-3)) (L (-7))) (N (L 19) (L 2))) == (1, 3)
-}