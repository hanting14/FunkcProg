{-# options_ghc -Wincomplete-patterns #-}

module Lesson02 where

-- Define the following functions in a type correct and total manner.
-- (No infinite loops or exceptions allowed.)

f1 :: (a, (b, (c, d))) -> (b, c)
f1 (a, (b, (c, d))) = (b, c)

f2 :: (a -> b) -> a -> b
f2 f x = f x

f3 :: (b -> c) -> (a -> b) -> a -> c
f3 g f x = g (f x)

f4 :: (a -> b -> c) -> (b -> a -> c)
f4 f x y = f y x

f5 :: ((a, b) -> c) -> (a -> b -> c)
f5 f x y = f(x, y)

f6 :: (a -> (b, c)) -> (a -> b, a -> c)
f6 f = (
    \a -> case (f a) of
        (b, c) -> b
    , 
    \a -> case (f a) of
        (b, c) -> c)

{-
f6 :: (a -> (b, c)) -> (a -> b, a -> c)
f6 f = f6 f = (\a -> fst (f a), \a -> snd (f a))
-}

f7 :: (a -> b, a -> c) -> (a -> (b, c))
f7 (f, g) x = (f x, g x)

f8 :: (Either a b -> c) -> (a -> c, b -> c)
f8 f = (\a -> f (Left a), \b -> f (Right b))

f9 :: (a -> c, b -> c) -> (Either a b -> c)
f9 (f, g) (Left a) = f a
f9 (f, g) (Right b) = g b

f10 :: Either (a, b) (a, c) -> (a, Either b c)
f10 (Left a) = (fst a, (Left (snd a)))
f10 (Right a) = (fst a, (Right (snd a)))

f11 :: (a, Either b c) -> Either (a, b) (a, c)
f11 (a, (Left x)) = (Left (a, x))
f11 (a, (Right x)) = (Right (a,x))

-- Extra (harder) task:
f12 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f12 = undefined


{- Algebraic Data Types (ADT) -}

data Color = Red | Green | Blue

data List a = Nil | Cons a (List a)
data Pair a b = MakePair a b
data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf _) = 0
height (Node l r) = 1 + (max (height l) (height r))

treeSum :: Tree Int -> Int
treeSum t = undefined


{- Type Class Instances -}

instance Show Color where
  show Red = "r"
  show Green = "g"
  show Blue = "b"

instance Show a => Show (List a) where
  show Nil = ""
  show (Cons x Nil) = show x
  show (Cons x xs) = show x ++ ", " ++ (show xs)

instance (Show a, Show b) => Show (Pair a b) where
  show (MakePair a' b') = "(" ++ (show a') ++ "," ++ (show b') ++ ")"

instance Show a => Show (Tree a) where
  show (Leaf x) = "(" ++ show x ++ ")"
  show (Node l r) = "[" ++ show l ++ " + " ++ show r ++ "]"

instance Eq a => Eq (Tree a) where
  (Leaf x) == (Leaf y) = x == y
  (Node l r) == (Node l' r') = l == l' && r == r'
  _ == _ = False

instance Eq Color where
  Red   == Red   = True
  Green == Green = True
  Blue  == Blue  = True
  _     == _     = False

instance Ord Color where
  Red   <= _     = True
  Green <= Green = True
  _     <= Blue  = True
  _     <= _     = False

-- class Eq a => Ord a where
--   (<=) :: a -> a -> Bool

instance Eq a => Ord (Tree a) where -- `Eq (Tree a)` is included!
  t <= t' = undefined

mapPair :: (a -> b) -> (c -> d) -> Pair a c -> Pair b d
mapPair pac = undefined

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe ma = undefined

mapList :: (a -> b) -> List a -> List b
mapList l = undefined

-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b

instance Functor List where
  fmap = undefined

instance Functor Tree where
  fmap = undefined