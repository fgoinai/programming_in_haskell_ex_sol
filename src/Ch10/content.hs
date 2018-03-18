module Ch10.Content where
import System.IO
import Data.Char

act :: IO (Char, Char)
act = do
    x <- getChar
    _ <- getChar
    y <- getChar
    return (x, y)

getLine' :: IO String
getLine' = do
    x <- getChar
    if x == '\n' then
        return []
    else
        do
            xs <- getLine'
            return (x:xs)

putStr' :: String -> IO ()
putStr' []      = return ()
putStr' (x:xs)  = do putChar x
                     putStr' xs

putStrLn' :: String -> IO ()
putStrLn' xs = do putStr' xs
                  putChar '\n'

strlen :: IO ()
strlen = do
    putStr' "Enter a string: "
    xs <- getLine'
    putStr' "The string has "
    putStr' (show (length xs))
    putStrLn' " characters"

-- hangman
hangman :: IO ()
hangman = do
  putStrLn "Think of a word:"
  word <- sgetLine
  putStrLn "Try to guess it:"
  playHangman word

sgetLine :: IO String
sgetLine = do
  x <- getCh
  if x == '\n' then
    do putChar x
       return []
  else
    do putChar '_'
       xs <- sgetLine
       return (x:xs)

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

playHangman :: String -> IO ()
playHangman word = do
  putStr "? "
  guess <- getLine
  if guess == word then
    putStrLn "You got it!!"
  else
    do putStrLn (match word guess)
       playHangman word

match :: String -> String -> String
match xs ys = [if elem x ys then x else '_' | x <- xs]

-- nim
next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
  where update r n = if r == row then n-num else n

putRow :: Int -> Int -> IO ()
putRow row num = do
  putStr (show row)
  putStr ": "
  putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do
  putRow 1 a
  putRow 2 b
  putRow 3 c
  putRow 4 d
  putRow 5 e

getDigit :: String -> IO Int
getDigit prompt = do
  putStr prompt
  x <- getChar
  newline
  if isDigit x then
    return (digitToInt x)
  else
    do putStrLn "ERROR: Invalid digit"
       getDigit prompt

newline :: IO ()
newline = putChar '\n'

playNim :: Board -> Int -> IO ()
playNim board player = do
  newline
  putBoard board
  if finished board then
    do newline
       putStr "Player "
       putStr (show (next player))
       putStrLn " wins!"
  else
    do newline
       putStr "Player "
       putStrLn (show player)
       row <- getDigit "Enter a row number: "
       num <- getDigit "Stars to remove: "
       if valid board row num then
        playNim (move board row num) (next player)
       else
        do newline
           putStrLn "ERROR: Invalid move"
           playNim board player

nim :: IO ()
nim = playNim initial 1

-- game of life
cls :: IO ()
cls = putStrLn "\ESC[2J"

type Pos = (Int, Int)

writeat :: Pos -> String -> IO ()
writeat p xs = do
  goto p
  putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

width :: Int
width = 10

height :: Int
height = 10

type Board' = [Pos]

glider :: Board'
glider = [(4,1),(2,3),(4,3),(3,4),(4,4),(5,5)]

showcells :: Board' -> IO ()
showcells b = sequence_ [writeat p "0" | p <- b]

isAlive :: Board' -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board' -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y), (x+1,y), (x-1,y+1), (x,y+1), (x+1,y+1)]

wrap :: Pos -> Pos
wrap (x,y) = ((mod (x-1) width) + 1, (mod (y-1) height + 1))

liveneighbs :: Board' -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board' -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3]]

births :: Board' -> [Pos]
-- births b = [(x,y) | x <- [1..width], y <- [1..height], isEmpty b (x,y), liveneighbs b (x,y) == 3]
births b = [p | p <- rmdups (concat (map neighbs b)), isEmpty b p, liveneighbs b p == 5]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nextgen :: Board' -> Board'
nextgen b = survivors b ++ births b

life :: Board' -> IO ()
life b = do
  cls
  showcells b
  wait 500000
  life (nextgen b)

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]