main :: IO ()
main = do
    putStrLn $ show $ luhnSum [1,8] [7,4]

halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve xs = (take (hl xs) xs, drop (hl xs) xs)
    where hl ls = floor $ (/2) $ toRational $ (length ls)

third :: [a] -> Maybe a
third [] = Nothing
third xs = -- b+c
    if length xs < 3
        then Nothing
        else Just (xs !! 2)
-- third xs = head $ tail $ tail xs --a

safetail :: [a] -> [a]
safetail xs | null xs   = []
            | otherwise = tail xs

(||) :: Bool -> Bool -> Bool
(||) False False = False
(||) _ _ = True

(&&) :: Bool -> Bool -> Bool
(&&) a b = 
    if a == True 
        then if b == True then True else False
        else False

mult :: Int -> Int -> Int -> Int
mult = \x -> \y -> \z -> x*y*z

luhnDouble :: Int -> Int
luhnDouble x = f $ x * 2
    where f y | y > 9 = y-9
              | otherwise = y

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = if mod (luhnSum [a,c] [b,d]) 10 == 0 then True else False

luhnSum :: [Int] -> [Int] -> Int
luhnSum (x:xs) ys = luhnDouble x + if xs == [] then sum ys else luhnSum xs ys