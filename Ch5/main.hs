import Data.Char

table :: [Float]
table = [8.1,1.5,2.8,4.2,12.7,2.2,2.0,6.1,7.0,0.2,0.8,4.0,2.4,6.7,7.5,1.9,0.1,6.0,6.3,9.0,2.8,1.0,2.4,0.2,2.0,0.1]

primes :: Int -> Int -> [Int]
primes start end = [y | y <- [start..end], factors y == [1,y]]
    where factors k = [x | x <- [1..k], mod k x == 0]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x' , i) <- zip xs [0..], x == x']

-- char part
char2int :: Char -> Int
char2int c = ord c - ord 'a'

int2char :: Int -> Char
int2char n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2char (mod (char2int c + n) 26)
          | otherwise = c

-- a(97) -> 3 -> d(100)
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

-- float part
percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
    where n = lowers xs

count :: Char -> String -> Int
count x xs = sum [1 | n <- xs, x == n]

lowers :: String -> Int
lowers xs = sum [1 | x <- xs, isLower x]

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

----------------------- crack
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- cal the stat to reverse the encode
crack :: String -> String
crack xs = encode (-factor) xs
    where
        factor = head (positions (minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n <- [0..26]]
        table' = freqs xs