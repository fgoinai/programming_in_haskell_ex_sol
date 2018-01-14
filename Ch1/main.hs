main :: IO ()
main = do
    putStrLn $ show $ sum' [1..10]
    putStrLn $ show $ product' [2..4]
    putStrLn $ show $ qsort_r [1..10]

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

qsort_r :: Ord a => [a] -> [a]
qsort_r [] = []
qsort_r (x:xs) = qsort_r left ++ [x] ++ qsort_r right
    where
        left = [a | a <- xs, a > x]
        right = [b | b <- xs, b <= x]