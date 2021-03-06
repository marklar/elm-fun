module Tree.BTree where

import Tree.Types

leaf :: a -> BTree a
leaf v = Node v Empty Empty

--------------------
-- traverse

-- Modify value in each node.
mapTree :: (a -> b) -> BTree a -> BTree b
mapTree f = foldTree (\v lRes rRes -> Node (f v) lRes rRes) Empty

-- PRE-order traversal.  (1. root.  2. left.  3. right.)
-- f's args: nodeVal leftResult rightResult
foldTree :: (a -> b -> b -> b) -> b -> BTree a -> b
foldTree f acc Empty = acc
foldTree f acc (Node v left right) =
    f v (foldTree f acc left) (foldTree f acc right)

tSum :: Num a => BTree a -> a
tSum = foldTree (\v lRes rRes -> v + lRes + rRes) 0

-- Number of values.
tSize :: BTree a -> Int
tSize = foldTree (\_ lRes rRes -> 1 + lRes + rRes) 0

-- Number of levels in the tree.
tDepth :: BTree a -> Int
tDepth = foldTree (\_ lRes rRes -> 1 + maximum [lRes, rRes]) 0
