{-
   You are given N nodes, each having unique value ranging from [1, N].
   How many different binary search trees can be created using all of them?
   Output: Print the answer modulo (10^8+7).
-}
module Main where

{-
   Give it plenty of space.
       $ ghc --make numBst -with-rtsopts "-K10g"
-}

import System.IO
import Control.Monad
import Data.List.Split (splitOn)

answer :: Integer -> Integer
answer a = a `mod` (10 ^ 8 + 7)

numsLeftRight :: Int -> [(Int,Int)]
numsLeftRight n = map pair indices
    where indices = [0 .. lastIdx]
          pair i = (i, lastIdx-i)
          lastIdx = n-1

memoNumTrees :: Int -> Integer
memoNumTrees = (map nt [0 ..] !!)
   where nt 0 = 1
         nt 1 = 1
         nt 2 = 2
         nt n = sum (map forPair leftRight)
             where 
               forPair (l,r) = memoNumTrees l * memoNumTrees r
               leftRight = numsLeftRight n

main =
    do
      putStr "num:\n"
      str <- getLine
      let n = read str :: Int
      print $ answer (memoNumTrees n)
