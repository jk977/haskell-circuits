module Utils where

tupleToList2 :: (a,a) -> [a]
tupleToList2 (x,y) = [x,y]

tupleToList3 :: (a,a,a) -> [a]
tupleToList3 (x,y,z) = [x,y,z]

bits :: [(Int,Int,Int)]
bits = [(x,y,z) | x <- bs, y <- bs, z <- bs] where
    bs = [0,1] :: [Int]
