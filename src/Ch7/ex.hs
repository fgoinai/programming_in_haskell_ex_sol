import Data.Char

-- ex1 [f x | x <- xs, p x]
ex1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
ex1 f p = map f . filter p

-- ex2
all' :: (a -> Bool) -> [Bool] -> Bool -- As requested by ex, [Bool] must be the snd param
all' _ =  foldr (\p1 p2 -> p1 && p2) True
-- I think the correct one should be this
all2 :: Eq a => (a -> Bool) -> [a] -> Bool
-- all2 f = (== []) . filter (== False) . map f -- this will iterate the whole list
all2 f xs = case xs of
                    [] -> True
                    (x:_) -> case f x of
                                False -> False -- when one false, result must be false
                                True -> all2 f xs

any' :: (a -> Bool) -> [Bool] -> Bool
any' _ = foldr (\p1 p2 -> p1 || p2) False

any2 :: Eq a => (a -> Bool) -> [a] -> Bool
any2 _ [] = False
any2 f (x:xs) | f x == True = True
              | otherwise = any2 f xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs) | f x == True = x : takeWhile' f xs
                    | otherwise = takeWhile' f xs

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f = filter ((== False) . f)

-- ex3
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (check f) []
    where
        check f' = \x -> \xs -> if f' x == True then x : xs else xs

-- ex4
dec2int :: [Int] -> Int
dec2int = foldl (\prev x -> x + prev * 10) 0

-- ex5
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f = \(a, b) -> f a b

-- ex6
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

chop8' :: [Int] -> [[Int]]
chop8' = unfold (== []) (take 8) (drop 8)

mapU :: (a -> b) -> [a] -> [b]
mapU f = unfold (null) (f . head) (tail)

iterateU :: (a -> a) -> a -> [a]
iterateU f = unfold (const False) id f

--ex7
--ori in book
bin2int :: [Int] -> Int
-- bin2int bits = sum [w*b | (w,b) <- zip weights bits]
--         where weights = iterate (*2) 1
bin2int = foldr (\x y -> x + 2*y) 0
-- bin2int [1,0,1,1] -> 13 -> (1*1 + 0*2 + 1*4 + 1*8)

int2bin :: Int -> [Int]
int2bin 0 = []
int2bin n = mod n 2 : int2bin (div n 2)

-- ensure int2bin convert to 8 digit
make8 :: [Int] -> [Int]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Int]
encode = concat . map (make8 . int2bin . ord)
-- char -> ASCII -> byte

chop8 :: [Int] -> [[Int]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits) --each 8 bits = 1 char, so [[8 bits]] = [char] = string

decode :: [Int] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . id . encode

-- 9th bit = parity bit, odd $ len $ filter odd bits = true -> 1, else 0
-- encode add, decode check
isCountOdd :: [Int] -> Bool
isCountOdd = odd . length . filter (\x -> odd x)

make9 :: [Int] -> [Int]
make9 bits = if isCountOdd (make8 bits) then make8 bits ++ [1] else make8 bits ++ [0]

encode9 :: String -> [Int]
encode9 = concat . map (make9 . int2bin . ord)

check9 :: [Int] -> [Int]
check9 xs = if (isCountOdd $ take 8 xs) == (odd $ last xs)
    then init xs
    else error "Parity bit is not correct"

chop9 :: [Int] -> [[Int]]
chop9 [] = []
chop9 bits = check9 (take 9 bits) : chop9 (drop 9 bits)

decode9 :: [Int] -> String
decode9 = map (chr . bin2int) . chop9

transmit9 :: String -> String
transmit9 = decode9 . id . encode9

--ex8
transmitErr :: String -> String
transmitErr = decode9 . tail . id . encode9

--ex9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f1 f2 (x:xs) | null xs = f1 x : []
                    | otherwise = f1 x : f2 (head xs) : altMap f1 f2 (tail xs)

--ex10
luhn :: [Int] -> Bool
luhn xs = mod (calSum xs) 10 == 0

calSum :: [Int] -> Int
calSum = sum . altMap (\x -> if x*2 > 10 then x*2-9 else x*2) id