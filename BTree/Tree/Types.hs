module Tree.Types where

{-
  * write a binary search tree implementation:
    - implement insert function
  * think about how you would:
    - store this binary search tree in a file, and
    - how you would read it back
  * implement some basic traversal functions: max, min, size

  * TODO
    - make BALANCED binary search tree
    - deleteMin :: BTree a -> BTree a
    - deleteMax :: BTree a -> BTree a
-}

data BTree a = Empty | Node a (BTree a) (BTree a)
               deriving (Eq, Show, Read)

-- What would this be useful for?
instance Functor BTree where
    fmap f Empty = Empty
    fmap f (Node v left right) = Node (f v) (fmap f left) (fmap f right)

data Crumb a = LeftCrumb a (BTree a) | RightCrumb a (BTree a)
               deriving (Show)
type Crumbs a = [Crumb a]
type Zipper a = (BTree a, Crumbs a)

