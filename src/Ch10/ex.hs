module Ch10.Ex where
import Data.Char
import System.IO
import Ch10.Content (Board, putRow, getDigit)

-- 1
putStr1 :: String -> IO ()
putStr1 [] = return ()
putStr1 xs = sequence_ [putChar x | x <- xs]

-- 2
boardToRow :: [(Int, Int)] -> IO ()
boardToRow [] = return ()
boardToRow (x:xs) = do
    putRow (fst x) (snd x)
    boardToRow xs

putBoard2 :: Board -> IO ()
putBoard2 [] = return ()
putBoard2 xs = boardToRow (zip [1..] xs)

-- 3
putBoard3 :: Board -> IO ()
putBoard3 [] = return ()
putBoard3 xs = sequence_ [putRow i x | (i,x) <- zip [1..] xs]

-- 4
getNumbers :: Int -> Int -> IO Int
getNumbers y 0 = return y
getNumbers y x = do
    d <- getDigit ""
    getNumbers (y+d) (x-1)

adder :: IO ()
adder = do
    d <- getDigit "How many numbers? "
    result <- getNumbers 0 d
    putStr "The total is "
    putStrLn $ show result

-- 5
getNumbers5 :: Int -> IO [Int]
getNumbers5 y = sequence [x | x <- [getDigit "" | _ <- [1..y]]]

adder5 :: IO ()
adder5 = do
    d <- getDigit "How many numbers? "
    result <- getNumbers5 d
    putStr "The result is "
    putStrLn $ show $ sum result

-- 6
getCh :: IO Char
getCh  = do hSetEcho stdin False
            c <- getChar
            hSetEcho stdin True
            return c

cache :: String -> IO String
cache xs = do
    x <- getCh
    if x == '\n' then do
        putChar x
        return xs
    else if x == '\DEL' then do
        putChar '\b'
        putChar ' '
        putChar '\b'
        if 1 == length xs then
            cache []
        else
            cache (init xs)
    else do
        putChar x
        cache (xs ++ [x])
    

readLine :: IO String
readLine = cache []