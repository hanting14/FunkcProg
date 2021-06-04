data SparseList a
  = Cons a (SparseList a)
  | Skip (SparseList a)
  | Nil

-- Task 1: Write a foldable instance for the sparse list (a list that doesn't
-- necessarily have elements at all indices) type given by the definition above.

instance Foldable SparseList where
  foldr f acc Nil = acc
  foldr f acc (Skip x) = foldr f acc x
  foldr f acc (Cons a x) = f a (foldr f acc x)

-- Task 2: Define composition for functions that do not always produce a value,
-- can sometimes fail and return nothing.
-- In case of a failure, it should be propagated to the final result.

composeMaybe :: (b -> Maybe c) -> (a -> Maybe b) -> (a -> Maybe c)
composeMaybe f g a = 
    case g a of
        Nothing -> Nothing
        (Just b) ->
            case f b of
                Nothing -> Nothing
                (Just c) -> (Just c)

{-
   Some example evaluations:
   - foldr (&&) True (Cons True (Skip (Cons False Nil))) == False
   - foldr (+) 0 (Cons 1 (Cons 4 (Skip (Cons 2 (Cons 8 (Skip Nil)))))) == 15
   - foldr (*) 1 (Skip (Cons 1 (Cons 4 (Cons 2 (Cons 8 (Skip Nil)))))) == 64
   - foldr (++) "" (Cons "lorem" (Skip (Skip (Cons "ipsum" (Cons "dolor" (Skip (Cons "sit" (Cons "amet" Nil)))))))) == "loremipsumdolorsitamet"
   - composeMaybe (Just . (*5)) (Just . (+3)) 5 == Just 40
   - composeMaybe (Just . (+2)) (Just . (*4)) 6 == Just 26
   - composeMaybe (Just . (+2)) (const Nothing) 7 == Nothing
   - composeMaybe (\b -> if b then Just "ok" else Nothing) (Just . not) True == Nothing
   - composeMaybe (\b -> if b then Just "ok" else Nothing) (Just . not) False == Just "ok"
-}