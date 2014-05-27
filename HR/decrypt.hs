{-
You are given a sequence of N integers.
Find the number of continuous sequences of integers such that their sum is zero.

For example if the sequence is: 
2, -2, 6, -6, 8
There are 3 such sequences
2, -2
6, -6
2, -2, 6, -6

-}

module Main where

import Data.List
import Control.Monad

readInt :: String -> Int
readInt s = read s :: Int

getNums :: IO [Int]
getNums =
    do
      (numLinesStr:lns) <- liftM lines getContents
      let numLines = readInt numLinesStr
      return $ map readInt lns

-----------

isZeroSum :: [Int] -> Bool
isZeroSum xs = sum xs == 0

nonNullSubseqs :: [Int] -> [[Int]]
nonNullSubseqs xs =
    filter (not . null) (subsequences xs)

zeroSumSubseqs :: [Int] -> [[Int]]
zeroSumSubseqs xs =
    filter isZeroSum (nonNullSubseqs xs)

-----------

countZeroSums :: [Int] -> Int
countZeroSums xs = snd (countZeroSums' xs)
countZeroSums' xs = foldl f (0,0) xs
    where f (accSum, accCnt) x =
              (newSum, accCnt + (if newSum == 0 then 1 else 0))
              where newSum = accSum + x

countAllZeroSums :: [Int] -> Int
countAllZeroSums xs = sum $ map countZeroSums (filter (not . null) (tails xs))

-----------

main =
    do
      nums <- getNums
      print $ countAllZeroSums nums
