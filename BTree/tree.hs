module Main where

import Types
import BTree
import BSearchTree
import Balanced
import Lists
import Display

t0 :: BTree Int
-- ()
t0 = Empty

t1 :: BTree Int
-- (4 (3 () ()) (7 5 10))
t1 = Node 4 (leaf 3) (Node 7 (leaf 5) (leaf 10))

t2 :: BTree Int
-- (20 (15 (8 7 11) (18 ()())) (118 (35 (33 () ()) (49 Empty 60)) (166 () ())))
t2 = Node 20
     (Node 15 (Node 8 (leaf 7) (leaf 11))
               (leaf 18))
     (Node 118 (Node 35 (leaf 33) (Node 49 Empty (leaf 60)))
               (leaf 166))

notBSTree :: BTree Int
notBSTree = Node 20
            (Node 15 (Node 8 (leaf 7) (leaf 11))
                      (leaf 140))
            (Node 118 (Node 35 (leaf 33) (Node 49 Empty (leaf 60)))
                      (leaf 166))


-- Num a -> for tSum
foo :: (Num a, Show a) => BTree a -> IO ()
foo t = 
    do
      putStrLn (show t)
      putStrLn $ "min: " ++ show (tMin t)
      putStrLn $ "max: " ++ show (tMax t)
      putStrLn $ "size: " ++ show (tSize t)
      putStrLn $ "depth: " ++ show (tDepth t)
      putStrLn $ "sum: " ++ show (tSum t)

main =
    do
      foo t0
      --
      foo t1
      --
      writeFile "t2" (show t2)
      str <- readFile "t2"
      let newT2 = read str :: BTree Int
      foo newT2
      pp newT2
      putStrLn "--------------"
      pp (insert 50 newT2)   -- iInsertVal
      putStrLn "--------------"
      putStrLn $ "t2 contains 6? " ++ show (contains 6 t2)
      putStrLn $ "t2 contains 18? " ++ show (contains 18 t2)
      putStrLn $ "pre: " ++ show (preOrdList t2)
      putStrLn $ "ord: " ++ show (inOrdList t2)
      putStrLn $ "post: " ++ show (postOrdList t2)
      putStrLn $ "fromPreList: " ++ show (preOrdList (fromPreList (preOrdList t2)))

      -- FIXME: unnecessarily includes '[' and ']'.
      writeFile "t2.txt" (show (preOrdList t2))
      str2 <- readFile "t2.txt"
      let intList = read str2 :: [Int]
      print $ fromPreList intList
      putStrLn $ "t0 is a BSTree: " ++ show (isBSTree t0)
      putStrLn $ "t1 is a BSTree: " ++ show (isBSTree t1)
      putStrLn $ "t2 is a BSTree: " ++ show (isBSTree t2)
      putStrLn $ "newT2 is a BSTree: " ++ show (isBSTree newT2)
      putStrLn $ "notBSTree is a BSTree: " ++ show (isBSTree notBSTree)

      putStrLn $ "t0 balance factor: " ++ show (balanceFactor t0)
      putStrLn $ "t1 balance factor: " ++ show (balanceFactor t1)
      putStrLn $ "t2 balance factor: " ++ show (balanceFactor t2)

      putStrLn "--------------------------------"
      pp (fromList [1..25])
      putStrLn "--------------------------------"
