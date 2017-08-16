main :: IO ()
main = do
    putStrLn "fuck"

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

factorial' :: (Num a, Enum a) => a -> a
factorial' n = product' [1..n]

average' :: [Int] -> Int
average' [] = 0
average' xs = div (sum' xs) (length xs)

last' :: [a] -> a
-- last' xs = head $ reverse xs
last' xs = xs !! (length xs - 1) -- like xs[xs.size()-1]

init' :: [a] -> [a]
-- init' xs = reverse $ tail $ reverse xs -- 1st
init' [] = [] -- 2nd
init' (x:xs) = [x] ++ init' right
    where
        right = if length xs == 1 then [] else xs