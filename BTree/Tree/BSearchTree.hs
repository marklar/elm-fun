module Tree.BSearchTree where

import Tree.Types
import Tree.BTree

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
