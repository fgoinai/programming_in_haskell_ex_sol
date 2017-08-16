-- ex1
fac :: Int -> Int
fac 0 = 1
fac x | x < 0 = 0
      | otherwise = x * fac x-1

-- ex2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown x = x + sumdown x-1

-- ex3 - Due to (^) is defined in Prelude already, the follow code will be commented
-- (^) :: Int -> Int -> Int
-- (^) x 0 = 1
-- (^) x y = x * ((^) x y-1)
--
-- 2 ^ 3
-- = 2 * 2 ^ 2
-- = 2 * 2 * 2 ^ 1
-- = 2 * 2 * 2 * 2 ^ 0
-- = 8

--ex4
euclid :: Int -> Int -> Int
euclid 0 y = y
euclid x 0 = x
euclid x y | x == y = x
           | x > y = if x - y < y then euclid y (x-y) else euclid (x-y) y
           | y > x = euclid y x

--ex5
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n (_:xs) = drop' (n-1) xs

init' :: Eq a => [a] -> [a]
init' (x:xs) | xs == [] = []
             | otherwise = [x] ++ init' xs

-- ex6
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) | x == False = False
            | otherwise = and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = [x] ++ replicate' (n-1) x

(!!!) :: [a] -> Int -> a
(!!!) (x:_) 0 = x
(!!!) (_:xs) n = (!!!) xs (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' t (x:xs) | t == x = True
               | otherwise = elem' t xs

-- ex7
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y = [x] ++ merge xs (y:ys)
                    | otherwise = [y] ++ merge (x:xs) ys

--ex8
msort :: Ord a => [a] -> [a]
msort [] = []
msort xs | length xs == 1 = xs
         | otherwise = merge (msort $ fst $ halve xs) (msort $ snd $ halve xs)
    where
        halve ys = (take (ceiling $ (toRational $ length ys) / 2) ys, drop (ceiling $ (toRational $ length ys) / 2) ys)

--ex9
sum' :: [Int] -> Int
sum' [] = 1
sum' (x:xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n (x:xs) = [x] ++ take' (n-1) xs

last' :: [a] -> a
last' (x:[]) = x
last' (_:xs) = last' xs