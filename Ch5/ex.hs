import Data.List
import Data.Char

ex1 :: [Int]
ex1 = [i^2 | i <- [1..100]]

grid :: Int -> Int -> [(Int, Int)]
grid x y = [(i, j) | i <- [0..x], j <- [0..y]]

ex2 :: [(Int, Int)]
ex2 = grid 1 2

--ex3
square :: Int -> [(Int, Int)]
square n = grid n n

--ex4
replicate' :: Int -> a -> [a]
replicate' n x = [x | i <- [1..n]]

--ex5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

--ex6
factors :: Int -> [Int]
factors x = [j | j <- [1..x], mod x j == 0]

perfects :: Int -> [Int]
perfects n = [i | i <- [1..n], sum (init $ factors i) == i]

-- BOSS!!! ex7
--[(x,y) | x <- [1,2], y <- [3,4]] == [(1,3), (1,4), (2,3), (2,4)] which has two generators
--change to two comperhensions with one generator using concat -> one comperhension contain one gen
--Deduce:
--[x | x<-[1,2]] - i
--[y | y<-[x+1,x+2]] - ii
--[ [(x,y) | y <- [x+1, x+2] | x <- [1..2] ]
ex7 = concat [ [(x,y) | y <- [x+1, x+2]] | x <- [1..2] ]

-- ex8
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], (find (== x') xs) == Just x]

--ex9
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * y | (x, y) <- zip xs ys]

--ex10
-- lowers -> engChars
engChars :: String -> Int
engChars xs = sum [1 | i <- xs, isLower i || isUpper i]
-- freqs, where n = engChars xs