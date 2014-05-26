module Tree.BSearchTree where

import Data.Maybe (fromJust)

import Tree.Types
import Tree.BTree
import Tree.Zipper

-- Promotes right.  (How to choose?)
zipDelete :: Ord a => a -> Zipper a -> Zipper a
zipDelete _ (Empty, cs)        = (Empty, cs)
zipDelete x z@(Node v l r, cs) = 
    case (x `compare` v) of
      LT -> (Node v l' r, cs)
          where (l', _) = zipDelete x (l, (LeftCrumb v r):cs)
      GT -> (Node v l r', cs)
          where (r', _) = zipDelete x (r, (RightCrumb v l):cs)
      EQ -> case (l,r) of
              (Empty, _) -> (l, cs)
              (_, Empty) -> (r, cs)
              (l, r) -> (Node maxLeft l' r, cs)
                  where
                    maxLeft = fromJust (tMax l)
                    (l', _) = zipDelete maxLeft (l, [])

-- Inserts a value into the tree (in the zipper -- we start at the top,
-- so we can safely assume that we need NOT go UP to insert).
-- If the value's already there, do NOT update.
-- Return the Zipper at the insertion point.
zipInsert :: Ord a => a -> Zipper a -> Zipper a
zipInsert x (Empty, cs) = (leaf x, cs)
zipInsert x z@(Node v left right, cs)
    | x == v    = z
    | x < v     = case goLeft z of
                    Nothing -> (leaf x, (LeftCrumb v right):cs)
                    Just z' -> zipInsert x z'
    | otherwise = case goRight z of
                    Nothing -> (leaf x, (RightCrumb v left):cs)
                    Just z' -> zipInsert x z'

--------------------

-- Highest val: always on far right.
tMax :: BTree a -> Maybe a
tMax = foldTree g Nothing
    where g v _ Nothing = Just v
          g _ _ rRes    = rRes

-- Lowest val: always on far left.
tMin :: BTree a -> Maybe a
tMin = foldTree g Nothing
    where g v Nothing _ = Just v
          g _ lRes _    = lRes

-- Is the value present?
-- TODO: reverse arg order?
contains :: Ord a => a -> BTree a -> Bool
contains x = foldTree g False
    where g v lRes rRes
            | x == v    = True
            | x <= v    = lRes
            | otherwise = rRes

-- Not a fold, since no need to traverse entirety of tree.
isBSTree :: Ord a => BTree a -> Bool
isBSTree = between (Nothing, Nothing)
    where between :: Ord a => (Maybe a, Maybe a) -> BTree a -> Bool
          between _ Empty = True
          between (min, max) (Node v left right) =
              and [ tween
                  , between (min, Just v) left
                  , between (Just v, max) right
                  ]
              where tween = case (min, max) of
                              (Nothing, Nothing) -> True
                              (Nothing, Just r)  -> v < r
                              (Just l, Nothing)  -> v >= l
                              (Just l, Just r)   -> v >= l && v < r
