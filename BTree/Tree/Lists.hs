module Tree.Lists where

import Tree.Types
import Tree.BTree
import Tree.Balanced (insert)

-- IN-order traversal.
-- 1. traverse left.  2. visit root.  3. traverse right.
inOrdList :: BTree a -> [a]
inOrdList = foldTree (\v lRes rRes -> lRes ++ v : rRes) []

-- POST-order traversal.
-- 1. traverse left.  2. traverse right.  3. visit root.
postOrdList :: BTree a -> [a]
postOrdList = foldTree (\v lRes rRes -> rRes ++ v : lRes) []

-- PRE-order traversal.
-- 1. visit root.  2. traverse left.  3. traverse right.
-- Not ordered.  aka 'flatten'
preOrdList :: BTree a -> [a]
preOrdList = foldTree (\v lRes rRes -> v : lRes ++ rRes) []

-- Not too efficient; with each folded-over `a`,
-- it starts at the root of the tree and moves down to insert.
fromList :: Ord a => [a] -> BTree a
fromList = foldl (flip insert) Empty

-- From >>pre<< OrdList, that is.
fromPreList :: Ord a => [a] -> BTree a
fromPreList []     = Empty
fromPreList (x:xs) =
    Node x (fromPreList lefts) (fromPreList rights)
    where lefts  = takeWhile (<= x) xs
          rights = dropWhile (<= x) xs
