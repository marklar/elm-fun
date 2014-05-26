module Tree.Zipper where

import Tree.Types
import Tree.BTree

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
