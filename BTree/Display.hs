module Display where

-- import Data.List.Split (splitOn)
import Data.List (intercalate)

import Types
import BTree

-- But I don't know how to make the read functions!
-- instance (Show a) => Show (BTree a) where
--     show Empty = "()"
--     show (Node v left right) =
--         intercalate " " ["(" ++ show v, show left, show right ++ ")"]

toSExp :: Show a => BTree a -> String
toSExp Empty = "()"
toSExp (Node v left right) =
    intercalate " " ["(" ++ show v, toSExp left, toSExp right ++ ")"]

pp :: Show a => BTree a -> IO ()
pp = (mapM_ putStrLn) . indentStrs

indentStrs :: Show a => BTree a -> [String]
indentStrs = foldTree g ["-- /-"]
    where g v ls (r:rs) = concat [ ["--" ++ show v]
                                 , map ("  |" ++) ls
                                 , ["  `" ++ r]
                                 , map ("   " ++) rs
                                 ]

tToFile :: Show a => FilePath -> BTree a -> IO ()
tToFile path = writeFile path . show

-- TODO: write to file in very compact way and read it back in.
-- Use Parsec for reading it back in?
