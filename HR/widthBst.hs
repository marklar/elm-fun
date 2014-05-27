
{-
  We store set of integers in a binary search tree serialized to a file.
  Calculate the width of the binary search tree. 
  == the number of nodes on the longest path between two terminal nodes (leaves) in the tree.
-}

{-
   Diameter: largest of:
     the diameter of T’s left subtree
     the diameter of T’s right subtree
     the longest path between leaves that goes through the root of T (this can be computed from the heights of the subtrees of T)
-}

{-
5
3
TERM
TERM
7
TERM
9
TERM
TERM
-}

module Main where

import Control.Monad

data BTree a = Empty | Node a (BTree a) (BTree a) deriving (Show, Eq)

-----------------------

readInt :: String -> Int
readInt s = read s :: Int

strToVal :: String -> Maybe Int
strToVal "TERM" = Nothing
strToVal s      = Just (readInt s)

valsFromLines :: [String] -> [Maybe Int]
valsFromLines lns = map strToVal lns

-----------------------

justVals :: [Maybe a] -> [a]
justVals [] = []
justVals (Nothing:xs)  = justVals xs
justVals ((Just x):xs) = [x] ++ justVals xs

-- From >>pre<< OrdList, that is.
fromPreList :: Ord a => [a] -> BTree a
fromPreList []     = Empty
fromPreList (x:xs) =
    Node x (fromPreList lefts) (fromPreList rights)
        where lefts  = takeWhile (<= x) xs
              rights = dropWhile (<= x) xs

getTree :: IO (BTree Int)
getTree =
    do 
      lns <- liftM lines getContents
      let maybeVals = valsFromLines lns
          vals = justVals maybeVals
      return $ fromPreList vals

-----------------------

-- PRE-order traversal.  (1. root.  2. left.  3. right.)
-- f's args: nodeVal leftResult rightResult
foldTree :: (a -> b -> b -> b) -> b -> BTree a -> b
foldTree f acc Empty = acc
foldTree f acc (Node v left right) =
    f v (foldTree f acc left) (foldTree f acc right)

-- Number of levels in the tree.
depth :: BTree a -> Int
depth = foldTree (\_ lRes rRes -> 1 + maximum [lRes, rRes]) 0

diameter :: BTree a -> Int
diameter Empty = 0
diameter (Node _ l r) =
    maximum [ 1 + depth l + depth r
            , diameter l
            , diameter r
            ]

main =
    do
      t <- getTree
      print $ diameter t

