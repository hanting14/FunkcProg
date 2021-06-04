data SparseList a
  = Cons a (SparseList a)
  | Skip (SparseList a)
  | Nil

-- Write a show instance for the sparse list (a list that doesn't necessarily
-- have elements at all indices) type given by the definition above based on the
-- example evaluations! (Separate by '|' and denote `Skip` with a hyphen!)

instance Show a => Show (SparseList a) where
  show Nil = "|"
  show (Cons x y) = "|" ++ show x ++ show y
  show (Skip x) = "|-" ++ show x

{-
   Some example evaluations:
   - show Nil                                 == "|"
   - show (Cons () (Skip (Skip Nil)))         == "|()|-|-|"
   - show (Cons True (Skip (Cons False Nil))) == "|True|-|False|"
   - show (Cons 1 (Cons 4 (Skip Nil)))        == "|1|4|-|"
   - show (Skip (Cons 'a' (Cons 'b' Nil)))    == "|-|'a'|'b'|"
-}