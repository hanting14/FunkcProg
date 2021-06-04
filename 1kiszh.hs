data Tree a = Leaf a | Node (Tree a) (Tree a)

-- Given the above definition of a tree type that stores values at its leaves
-- write a function that calculates the sum of numbers in an integer tree.

treeSum :: Tree Int -> Int
treeSum (Leaf a) = a
treeSum (Node x y) = treeSum x + treeSum y

{-
   Some example evaluations:
   - treeSum (Leaf 3) == 3
   - treeSum (Node (Leaf 5) (Leaf 2)) == 7
   - treeSum (Node (Leaf 6) (Node (Leaf 1) (Leaf 4))) == 11
-}