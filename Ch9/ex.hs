module Ch9.Ex where
import Ch9.Content (Op(Add,Sub,Mul,Div), Expr(Val,App), eval, exprs, apply, split, Result, combine, choices)

-- 1
-- from book
subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
                where
                    yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms []        = [[]]
perms (x:xs)    = concat (map (interleave x) (perms xs))
{-
choices :: [a] -> [[a]]
choices = concat . map perms . subs
-}
choices' :: [a] -> [[a]]
choices' xs = [y | x <- subs xs, y <- perms x]

-- 2
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True -- x:xs:[]
isChoice (_:_) [] = False
isChoice (x:xs) ys = elem x ys && isChoice xs (removeFirst x ys)

removeFirst :: Eq a => a -> [a] -> [a]
removeFirst x [] = []
removeFirst x (y:ys) | x == y    = ys
                     | otherwise = y : removeFirst x ys

-- 3
-- dead loop, the program will not stop

-- 4
-- get possible case count
possible :: [Int] -> [Expr]
possible l = [y | x <- choices' l, y <- exprs x]

possibleCount :: [Int] -> Int
possibleCount = length . possible

-- valid options
validOptions :: [Int] -> Int
validOptions l = length [i | i <- [eval z | y <- [exprs x | x <- choices' l, x /= []], z <- y], i /= []]

-- faster version
validOptions2 :: [Int] -> Int
validOptions2 = length . filter (not . null) . map eval . possible

-- 5
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub _ _ = True
valid Mul _ _ = True
valid Div x y = y /= 0 && mod x y == 0  -- As now include 0

eval5 :: Expr -> [Int]
eval5 (Val n)        = [n | n > 0]
eval5 (App o l r)    = [apply o x y | x <- eval5 l, y <- eval5 r, valid o x y]

validOptions5 :: [Int] -> Int
validOptions5 = length . filter (not . null) . map eval5 . possible

-- 6
data Op' = Add' | Sub' | Mul' | Div' | Exp'
instance Show Op' where
    show Add' = "+"
    show Sub' = "-"
    show Mul' = "*"
    show Div' = "/"
    show Exp' = "^"

valid' :: Op' -> Int -> Int -> Bool
valid' Add' x y = x <= y -- as ordering is requested
valid' Sub' x y = x > y
valid' Mul' x y = x /= 1 && y /= 1 && x < y
valid' Div' x y = y /= 0 && mod x y == 0
valid' Exp' x y = x /= 1 && y > 1

apply' :: Op' -> Int -> Int -> Int
apply' Add' x y = x + y
apply' Sub' x y = x - y
apply' Mul' x y = x * y
apply' Div' x y = div x y
apply' Exp' x y = x ^ y

data Expr' = Val' Int | App' Op' Expr' Expr'
instance Show Expr' where
    show (Val' n)        = show n
    show (App' o l r)    = brak l ++ show o ++ brak r
                            where -- bracket
                                brak (Val' n) = show n
                                brak e       = "(" ++ show e ++ ")"

type Result' = (Expr',Int)

ops :: [Op']
ops = [Add',Sub',Mul',Div',Exp']

combine'' :: Result' -> Result' -> [Result']
combine'' (l,x) (r,y) = [(App' o l r, apply' o x y) | o <- ops, valid' o x y]

results6 :: [Int] -> [Result']
results6 []  = []
results6 [n] = [(Val' n,n) | n > 0]
results6 ns  = [res | (ls,rs) <- split ns,
                      lx      <- results6 ls,
                      ry      <- results6 rs,
                      res     <- combine'' lx ry]

diff6 :: [Int] -> Int -> [Result']
diff6 ns n = [(e, abs $ m-n) | ns' <- choices' ns, (e,m) <- results6 ns']

-- qsort
order6 :: [Result'] -> [Result']
order6 [] = []
order6 (n:ns) = order6 l ++ (n : order6 r)
    where
        l = [x | x <- ns, snd x <= snd n]
        r = [x | x <- ns, snd x > snd n]

solution6 :: [Int] -> Int -> [Expr']
solution6 ns n = filMin [x | x <- order6 $ diff6 ns n]
        where
            filMin (x:xs) = [fst y | y <- (x:xs), snd y == snd x]