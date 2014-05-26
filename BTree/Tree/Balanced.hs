module Tree.Balanced where
-- AVL trees

import Tree.Types
import Tree.BTree
import Tree.Zipper
import Tree.BSearchTree

insert :: Ord a => a -> BTree a -> BTree a
insert x tree = newTree
    where (newTree, _) = goTopBalancing $ zipInsert x (tree, [])

delete :: Ord a => a -> BTree a -> BTree a
delete x tree = newTree
    where (newTree, _) = goTopBalancing $ zipDelete x (tree, [])

-----

goTopBalancing :: Zipper a -> Zipper a
goTopBalancing (t, []) = (balance t, [])
goTopBalancing zipper =
    case goUp zipper of
      Nothing      -> zipper   -- this shouldn't happen!
      Just (t, cs) -> goTopBalancing (balance t, cs)

-- by height.
-- perform tree rotations.
balance :: BTree a -> BTree a
balance Empty = Empty
balance t@(Node v l r) =
    case balanceFactor t of
      2    -> rotateRight (if balanceFactor l == (-1)
                           then (Node v (rotateLeft l) r)
                           else t)
      (-2) -> rotateLeft (if balanceFactor r == 1
                          then (Node v l (rotateRight r))
                          else t)
      otherwise -> t

balanceFactor :: BTree a -> Int
balanceFactor Empty = 0
balanceFactor (Node _ l r) = tDepth l - tDepth r

-- Optimization...
-- I think we're wasting effort here, recalculating the depth all the time.
-- After we calculate it once, we should know what it is for that subtree.
-- If we can include the info as we travel up, we can save some traversal time.
-- In other words, we balance a node, and we return the node AND its depth.

{-
     1            2
    / \          / \
   A   2        1   C
      / \      / \
     B   C    A   B
-}
rotateLeft :: BTree a -> BTree a
rotateLeft Empty = Empty
rotateLeft t@(Node _ _ Empty) = t
rotateLeft (Node v1 a (Node v2 b c)) = (Node v2 (Node v1 a b) c)

{-
        2        1
       / \      / \
      1   C    A   2
     / \          / \
    A   B        B   C
-}
rotateRight :: BTree a -> BTree a
rotateRight Empty = Empty
rotateRight t@(Node _ Empty _) = t
rotateRight (Node v2 (Node v1 a b) c) = (Node v1 a (Node v2 b c))
