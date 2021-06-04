data Result a = Ok a | Err String deriving (Eq, Show)

instance Functor Result where
  fmap f (Ok v) = Ok (f v)
  fmap f (Err e) = Err e

instance Applicative Result where
  pure = Ok
  (Ok f)  <*> ra = fmap f ra
  (Err e) <*> _  = Err e

instance Monad Result where
  (Ok v)  >>= f = f v
  (Err e) >>= _ = Err e

-- Task 1: Write a function that zips together two lists using a function that
--         might return an error instead of a value. In that case the final
--         result should be an error as well, otherwise it should be the result.

zipWithResult :: (a -> b -> Result c) -> [a] -> [b] -> Result [c]
zipWithResult _ [] _          = pure []
zipWithResult _ _ []          = pure []
zipWithResult f (a:as) (b:bs) = do
    x <- f a b
    y <- zipWithResult f as bs
    pure (x:y)

-- Task 2: Write a function that zips together two lists, which might contain
--         errors instead of values. In case an error is encountered, the
--         result of the entire operation should be an error.

zipWithResult' :: (a -> b -> c) -> [Result a] -> [Result b] -> Result [c]
zipWithResult' f [] _  = pure []
zipWithResult' f _  [] = pure []
zipWithResult' f (a:as) (b:bs) = do
    a' <- a
    b' <- b
    rest <- zipWithResult' f as bs
    pure ((f a' b'):rest)

{-
  Some example evaluations:
  - zipWithResult (\x y -> Ok (x + y)) [1, 4, 5] [3, 7, 9] == Ok [4, 11, 14]
  - zipWithResult (\x y -> Err "error") [1, 4, 5] [3, 7, 9] == (Err "error" :: Result [Int])
  - zipWithResult (\x y -> if even (x + y) then Ok (x * y) else Err "odd") [1, 3, 4] [3, 7, 6] == Ok [3, 21, 24]
  - zipWithResult (\x y -> if even (x + y) then Ok (x * y) else Err "odd") [1, 4, 5] [3, 7, 9] == Err "odd"

  - zipWithResult' (+) [Ok 1, Ok 4, Ok 5] [Ok 3, Ok 7, Ok 9] == Ok [4, 11, 14]
  - zipWithResult' (+) [Ok 1, Err "NaN", Ok 5] [Err "NaN", Ok 7, Err "NaN"] == Err "NaN"
  - zipWithResult' (,) [Ok 'a', Ok 'd'] [Ok 's', Ok 'f'] == Ok [('a', 's'), ('d', 'f')]
  - zipWithResult' (,) [Err "error", Ok 'd'] [Ok 's', Ok 'f'] == Err "error"
-}