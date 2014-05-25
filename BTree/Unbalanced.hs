module Unbalanced where

-- Inserts a value into the tree.
-- If it's already there, do NOT update.
-- TODO: reverse arg order?
tInsertVal :: Ord a => a -> BTree a -> BTree a
tInsertVal x Empty = leaf x
tInsertVal x (Node v left right)
    | x == v    = Node v left right
    | x < v     = Node v (tInsertVal x left) right
    | otherwise = Node v left (tInsertVal x right)

-- Takes one SBTree and inserts it into another.
-- (Useful for deleting values from a tree.)
tInsertTree :: Ord a => BTree a -> BTree a -> BTree a
tInsertTree ta Empty = ta
tInsertTree Empty tb = tb
tInsertTree ta@(Node a _ _) (Node b left right)
    | a <= b    = Node b (tInsertTree ta left) right
    | otherwise = Node b left (tInsertTree ta right)

-- If not found, just return the same tree.
tDelete :: Ord a => a -> BTree a -> BTree a
tDelete x Empty = Empty
tDelete x t@(Node v Empty Empty)
    | x == v    = Empty
    | otherwise = t
tDelete x (Node v left right)
    | x > v     = Node v left (tDelete x right)
    | x < v     = Node v (tDelete x left) right
    | otherwise = tInsertTree left right  -- Promotes right.  (How to choose?)


