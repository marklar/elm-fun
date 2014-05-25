module Main where

import Data.List (intercalate)
import Data.List.Split (splitOn)

{-
  * write a binary search tree implementation:
    - implement insert function
  * think about how you would:
    - store this binary search tree in a file, and
    - how you would read it back
  * implement some basic traversal functions: max, min, size

  * TODO
    - make BALANCED binary search tree
    - isBSTree :: BSTree a -> Bool
    - deleteMin :: BSTree a -> BSTree a
    - deleteMax :: BSTree a -> BSTree a
-}


data BSTree a = Empty | Node a (BSTree a) (BSTree a)
                deriving (Eq, Show, Read)

leaf :: a -> BSTree a
leaf v = Node v Empty Empty

-------------------------------------

data Crumb a = LeftCrumb a (BSTree a) | RightCrumb a (BSTree a) deriving (Show)
type Crumbs a = [Crumb a]
type Zipper a = (BSTree a, Crumbs a)

-- chain these fns with >>=

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

-- by height.
-- perform tree rotations.
balance :: BSTree a -> BSTree a
balance Empty = Empty
balance t@(Node v l r) =
    case balanceFactor t of
      2    -> rotateRight (if balanceFactor l == (-1)
                           then (Node v (rotateLeft l) r)
                           else t)
      (-2) -> rotateLeft (if balanceFactor r == 1
                          then (Node v l (rotateRight r))
                          else t)
      otherwise -> t

balanceFactor :: BSTree a -> Int
balanceFactor Empty = 0
balanceFactor (Node _ l r) = tDepth l - tDepth r

-- Optimization...
-- I think we're wasting effort here, recalculating the depth all the time.
-- After we calculate it once, we should know what it is for that subtree.
-- If we can include the info as we travel up, we can save some traversal time.
-- In other words, we balance a node, and we return the node AND its depth.

{-
     1            2
    / \          / \
   A   2        1   C
      / \      / \
     B   C    A   B
-}
rotateLeft :: BSTree a -> BSTree a
rotateLeft Empty = Empty
rotateLeft t@(Node _ _ Empty) = t
rotateLeft (Node v1 a (Node v2 b c)) = (Node v2 (Node v1 a b) c)

{-
        2        1
       / \      / \
      1   C    A   2
     / \          / \
    A   B        B   C
-}
rotateRight :: BSTree a -> BSTree a
rotateRight Empty = Empty
rotateRight t@(Node _ Empty _) = t
rotateRight (Node v2 (Node v1 a b) c) = (Node v1 a (Node v2 b c))

insert :: Ord a => a -> BSTree a -> BSTree a
insert x tree = newTree
    where (newTree, _) = goTopBalancing $ zipInsertVal x (tree, [])

goTopBalancing :: Zipper a -> Zipper a
goTopBalancing (t, []) = (balance t, [])
goTopBalancing zipper =
    case goUp zipper of
      Nothing      -> zipper   -- this shouldn't happen!
      Just (t, cs) -> goTopBalancing (balance t, cs)

-------------------------------------

-- What would this be useful for?
instance Functor BSTree where
    fmap f Empty = Empty
    fmap f (Node v left right) = Node (f v) (fmap f left) (fmap f right)

-- But I don't know how to make the read functions!
-- instance (Show a) => Show (BSTree a) where
--     show Empty = "()"
--     show (Node v left right) =
--         intercalate " " ["(" ++ show v, show left, show right ++ ")"]

toSExp :: Show a => BSTree a -> String
toSExp Empty = "()"
toSExp (Node v left right) =
    intercalate " " ["(" ++ show v, toSExp left, toSExp right ++ ")"]

--------------------
-- display

pp :: Show a => BSTree a -> IO ()
pp = (mapM_ putStrLn) . indentStrs

indentStrs :: Show a => BSTree a -> [String]
indentStrs = foldTree g ["-- /-"]
    where g v ls (r:rs) = concat [ ["--" ++ show v]
                                 , map ("  |" ++) ls
                                 , ["  `" ++ r]
                                 , map ("   " ++) rs
                                 ]

tToFile :: Show a => FilePath -> BSTree a -> IO ()
tToFile path = writeFile path . show

-- TODO: write to file in very compact way and read it back in.
-- Use Parsec for reading it back in?

-- Not a fold, since no need to traverse entirety of tree.
isBSTree :: Ord a => BSTree a -> Bool
isBSTree = between (Nothing, Nothing)
    where between :: Ord a => (Maybe a, Maybe a) -> BSTree a -> Bool
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

--------------------
-- update

-- Inserts a value into the tree.
-- If it's already there, do NOT update.
-- TODO: reverse arg order?
tInsertVal :: Ord a => a -> BSTree a -> BSTree a
tInsertVal x Empty = leaf x
tInsertVal x (Node v left right)
    | x == v    = Node v left right
    | x < v     = Node v (tInsertVal x left) right
    | otherwise = Node v left (tInsertVal x right)

-- Takes one SBTree and inserts it into another.
-- (Useful for deleting values from a tree.)
tInsertTree :: Ord a => BSTree a -> BSTree a -> BSTree a
tInsertTree ta Empty = ta
tInsertTree Empty tb = tb
tInsertTree ta@(Node a _ _) (Node b left right)
    | a <= b    = Node b (tInsertTree ta left) right
    | otherwise = Node b left (tInsertTree ta right)

-- If not found, just return the same tree.
tDelete :: Ord a => a -> BSTree a -> BSTree a
tDelete x Empty = Empty
tDelete x t@(Node v Empty Empty)
    | x == v    = Empty
    | otherwise = t
tDelete x (Node v left right)
    | x > v     = Node v left (tDelete x right)
    | x < v     = Node v (tDelete x left) right
    | otherwise = tInsertTree left right  -- Promotes right.  (How to choose?)


--------------------
-- traverse

-- Modify value in each node.
mapTree :: (a -> b) -> BSTree a -> BSTree b
mapTree f = foldTree (\v lRes rRes -> Node (f v) lRes rRes) Empty

-- f's args: nodeVal leftResult rightResult
foldTree :: (a -> b -> b -> b) -> b -> BSTree a -> b
foldTree f acc Empty = acc
-- PRE-order traversal.
-- 1. visit root.  2. traverse left.  3. traverse right.
foldTree f acc (Node v left right) =
    f v (foldTree f acc left) (foldTree f acc right)

-- IN-order traversal.
-- 1. traverse left.  2. visit root.  3. traverse right.
inOrdList :: BSTree a -> [a]
inOrdList = foldTree (\v lRes rRes -> lRes ++ v : rRes) []

-- POST-order traversal.
-- 1. traverse left.  2. traverse right.  3. visit root.
postOrdList :: BSTree a -> [a]
postOrdList = foldTree (\v lRes rRes -> rRes ++ v : lRes) []

-- PRE-order traversal.
-- 1. visit root.  2. traverse left.  3. traverse right.
-- Not ordered.  aka 'flatten'
preOrdList :: BSTree a -> [a]
preOrdList = foldTree (\v lRes rRes -> v : lRes ++ rRes) []

-- Not too efficient; with each folded-over `a`,
-- it starts at the root of the tree and moves down to insert.
fromList :: Ord a => [a] -> BSTree a
fromList = foldl (flip insert) Empty

-- From >>pre<< OrdList, that is.
fromPreList :: Ord a => [a] -> BSTree a
fromPreList []     = Empty
fromPreList (x:xs) =
    Node x (fromPreList lefts) (fromPreList rights)
    where lefts  = takeWhile (<= x) xs
          rights = dropWhile (<= x) xs

tSum :: Num a => BSTree a -> a
tSum = foldTree (\v lRes rRes -> v + lRes + rRes) 0

-- Is the value present?
-- TODO: reverse arg order?
contains :: Ord a => a -> BSTree a -> Bool
contains x = foldTree g False
    where g v lRes rRes
            | x == v    = True
            | x <= v    = lRes
            | otherwise = rRes

-- Highest val: always on far right.
tMax :: BSTree a -> Maybe a
tMax = foldTree g Nothing
    where g v _ Nothing = Just v
          g _ _ rRes    = rRes

-- Lowest val: always on far left.
tMin :: BSTree a -> Maybe a
tMin = foldTree g Nothing
    where g v Nothing _ = Just v
          g _ lRes _    = lRes

-- Number of values.
tSize :: BSTree a -> Int
tSize = foldTree (\_ lRes rRes -> 1 + lRes + rRes) 0

-- How many nodes are traversed to get to bottom leaf?
tDepth :: BSTree a -> Int
tDepth = foldTree (\_ lRes rRes -> 1 + maximum [lRes, rRes]) 0

--------------------
--------------------

t0 :: BSTree Int
-- ()
t0 = Empty

t1 :: BSTree Int
-- (4 (3 () ()) (7 5 10))
t1 = Node 4 (leaf 3) (Node 7 (leaf 5) (leaf 10))

t2 :: BSTree Int
-- (20 (15 (8 7 11) (18 ()())) (118 (35 (33 () ()) (49 Empty 60)) (166 () ())))
t2 = Node 20
     (Node 15 (Node 8 (leaf 7) (leaf 11))
               (leaf 18))
     (Node 118 (Node 35 (leaf 33) (Node 49 Empty (leaf 60)))
               (leaf 166))

notBSTree :: BSTree Int
notBSTree = Node 20
     (Node 15 (Node 8 (leaf 7) (leaf 11))
               (leaf 140))
     (Node 118 (Node 35 (leaf 33) (Node 49 Empty (leaf 60)))
               (leaf 166))


-- Num a -> for tSum
foo :: (Num a, Show a) => BSTree a -> IO ()
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
      let newT2 = read str :: BSTree Int
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
