module Ch11.Ex where
import Ch11.Content (moves, next, Tree(Node), Player(O,B,X), Grid, minimax, prune, gametree, size, depth, putGrid, bestmove, wins, turn, getNat, empty, full, prompt, move)
import Ch10.Content (goto, cls)
import System.Random (randomRIO)
import Data.Char
import System.IO
-- import Data.Array (listArray, (!))

raw = [[B,B,B],[B,B,B],[B,B,B]]
eg1 = [[B,B,O],[B,O,B],[B,B,X]]

-- 1
gametreeCount :: Grid -> Player -> Int
gametreeCount g p = 1 + sum [gametreeCount g' (next p) | g' <- moves g p]

gametreeCount' :: Tree Grid -> Int
gametreeCount' (Node _ []) = 1
gametreeCount' (Node _ ts) = 1 + sum [gametreeCount' t | t <- ts]

-- compile with stack ghc -- -O2 -main-is Ch11.Ex -o Ch11/ex Ch11/ex.hs
-- run like a rocket :)
-- main :: IO ()
-- main = putStr $ show $ gametreeCount raw O
-- main = putStr $ show $ gametreeCount' $ gametree raw O

-- 2
goodmoves :: Grid -> Player -> [Grid]
goodmoves g p = [g' | Node (g',p') _ <- ts, p' == best]
    where
        Node (_,best) ts = minimax $ prune depth $ gametree g p

randomBestMove :: Grid -> Player -> IO Grid
randomBestMove g p = do
    let ls = goodmoves g p
    x <- randomRIO (0, length ls - 1)
    return $ ls !! fromIntegral x

-- 3
minimaxWithLeastDepth :: Tree Grid -> Tree (Grid, Player, Int)
minimaxWithLeastDepth (Node g [])
    | wins O g = Node (g,O,1) []
    | wins X g = Node (g,X,1) []
    | otherwise = Node (g,B,1) []
minimaxWithLeastDepth (Node g ts)
    | turn g == O = Node (g, minimum ps, minimum cs) ts'
    | turn g == X = Node (g, maximum ps, minimum cs) ts'
        where
            ts' = map minimaxWithLeastDepth ts
            raw = [(p,c) | Node (_,p,c) _ <- ts']
            ps = [p | (p,_) <- raw]
            cs = [c + 1 | (_,c) <- raw]

type GridWDepth = (Grid,Int)

leastDepth :: [GridWDepth] -> GridWDepth
leastDepth [] = ([],0)
leastDepth (x:xs) = checkBranchDepth x xs

checkBranchDepth :: GridWDepth -> [GridWDepth] -> GridWDepth
checkBranchDepth m [] = m
checkBranchDepth m (y:ys) | snd y < snd m = checkBranchDepth y ys
                          | otherwise = checkBranchDepth m ys

leastDepthBestmove :: Grid -> Player -> Grid
leastDepthBestmove g p = fst $ leastDepth [(g',c) | Node (g',p',c) _ <- ts, p' == best]
            where
                tree = prune depth (gametree g p)
                Node (_,best,_) ts = minimaxWithLeastDepth tree

-- 4
-- let player decide play first or second
-- allow custom win condition, eg. 4 in a chain to win -> 4X4
-- persistance game tree instead of generate it every time
-- reduce the size of game tree by using alpha-beta pruning
playerOrderDecision :: IO Player
playerOrderDecision = do
    putStr "Please select role: O (first) / X (second): "
    x <- getLine
    -- let x = xs !! 0
    if x /= "O" && x /= "X" then do
        putStrLn "Only O/X is accepted!"
        playerOrderDecision
    else
        if x == "O" then
            return O
        else
            return X

sizeDecision :: IO Int
sizeDecision = getNat "Please provide win condition, i.e. numbers of chess required in a chain to win\nThis will also affect the base size"


-- gen gametree at the beginning without prune, access it like pointer
-- alpha-beta embedded into minimax
-- store gametree instance in play, return relative Tree node and match the option with the branch
play :: Player -> Grid -> Player -> IO()
play human g p = do
    cls
    goto (1,1)
    putGrid g
    play' human g p

-- use closure + curry to cache player order selection
play' :: Player -> Grid -> Player -> IO ()
play' human g p
    | wins human g = putStrLn "Player wins!\n"
    | wins (next human) g = putStrLn "AI wins!\n"
    | full g = putStrLn "It's a draw!\n"
    | p == human = do
        i <- getNat (prompt p)
        case move g i p of
            [] -> do
                putStrLn "ERROR: Invalid move"
                play' human g p
            [g'] -> f g' (next p)
    | p == (next human) = do
        putStr "AI is thinking..."
        (f $! bestmove g p) (next p)
    where
        f = play human

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    human <- playerOrderDecision
    case human of
        O -> play human empty O
        X -> do
            g <- randomBestMove empty O
            play human g X
        otherwise -> do
            putStrLn "Only O/X allowed, please choose again!"
            main
