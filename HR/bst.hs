{-
  Given a list of numbers, determine whether it can represent
  the preorder traversal of a binary search tree(BST).
-}
module Main where

import System.IO
import Control.Monad
import Data.List.Split (splitOn)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

------------------

mkPairs :: [String] -> [(String, String)]
mkPairs [] = []
mkPairs (_:[]) = []
mkPairs (a:b:xs) = [(a,b)] ++ mkPairs xs

readInt :: String -> Int
readInt s = read s :: Int

mkIntList :: String -> [Int]
mkIntList s = map readInt (splitOn " " s)

-- from stdin
getLists :: IO [[Int]]
getLists =
    do
      numStr:otherStrs <- liftM lines getContents
      let pairs = mkPairs otherStrs
          intLists = map mkIntList (map snd pairs)
      return intLists

------------------

-- From >>pre<< OrdList, that is.
fromPreList :: Ord a => [a] -> Tree a
fromPreList []     = Empty
fromPreList (x:xs) =
    Node x (fromPreList lefts) (fromPreList rights)
    where lefts  = takeWhile (<= x) xs
          rights = dropWhile (<= x) xs

-- Not a fold, since no need to traverse entirety of tree.
isBSTree :: Ord a => Tree a -> Bool
isBSTree = between (Nothing, Nothing)
    where between :: Ord a => (Maybe a, Maybe a) -> Tree a -> Bool
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

-- 1. Create a tree with the assumption that the list is in PRE-order.
-- 2. Observe whether it's in Search order.

------------------

data YesNo = YES | NO deriving (Show)
toYesNo :: Bool -> YesNo
toYesNo True  = YES
toYesNo False = NO

main =
    do
      intLists <- getLists
      let results = map (isBSTree . fromPreList) intLists
      mapM_ print (map toYesNo results)
