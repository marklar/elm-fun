module Tree.Zipper where

import Tree.Types
import Tree.BTree

-- Inserts a value into the tree (in the zipper -- we start at the top,
-- so we can safely assume that we need NOT go UP to insert).
-- If the value's already there, do NOT update.
-- Return the Zipper at the insertion point.
zipInsertVal :: Ord a => a -> Zipper a -> Zipper a
zipInsertVal x (Empty, cs) = (leaf x, cs)
zipInsertVal x z@(Node v left right, cs)
    | x == v    = z
    | x < v     = case goLeft z of
                    Nothing -> (leaf x, (LeftCrumb v right):cs)
                    Just z' -> zipInsertVal x z'
    | otherwise = case goRight z of
                    Nothing -> (leaf x, (RightCrumb v left):cs)
                    Just z' -> zipInsertVal x z'

-- Can chain these fns with >>=.

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Empty, _) = Nothing
goLeft (Node v l r, cs) = Just (l, (LeftCrumb v r):cs)

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Empty, _) = Nothing
goRight (Node v l r, cs) = Just (r, (RightCrumb v l):cs)

goUp :: Zipper a -> Maybe (Zipper a)
goUp (_, []) = Nothing
goUp (l, (LeftCrumb v r):cs)  = Just (Node v l r, cs)
goUp (r, (RightCrumb v l):cs) = Just (Node v l r, cs)

goTop :: Zipper a -> Zipper a
goTop (t, []) = (t, [])
goTop zipper =
    case goUp zipper of
      Nothing -> zipper   -- this shouldn't happen!
      Just z  -> goTop z
