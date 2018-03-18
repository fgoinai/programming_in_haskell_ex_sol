module Ch9.Content where
-- countdown problem
-- eg. 1,3,7,10,25,50 -> 765
-- 1. (1+50) * (25-10)
-- 2. ...

-- Define operator first
data Op = Add | Sub | Mul | Div
instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

-- +N op +N -> +N?
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = mod x y == 0

-- if valid, execute it
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = div x y

-- expression: Either[Int, Op]
data Expr = Val Int | App Op Expr Expr
instance Show Expr where
    show (Val n)        = show n
    show (App o l r)    = brak l ++ show o ++ brak r
                            where -- bracket
                                brak (Val n) = show n
                                brak e       = "(" ++ show e ++ ")"

-- list of Val
values :: Expr -> [Int]
values (Val n)      = [n]
values (App _ l r)  = values l ++ values r

-- evaluate expression and get final value
eval :: Expr -> [Int]
eval (Val n)        = [n | n > 0] -- gen a single item list with item's value > 0
eval (App o l r)    = [apply o x y | x <- eval l, y <- eval r, valid o x y]

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
                where
                    yss = subs xs
{-
[1,2,3] === 1:2:3:[], (x:) is list con fn
subs [1,2,3] ->
subs [2,3] ++ map (1:) (subs [2,3]) ->
subs [3] ++ map (2:) (subs [3]) ++ map (1:) (subs [3] ++ map (2:) (subs [3])) ->
subs [] ++ map (3:) (subs []) 
 ++ map (2:) (subs [] ++ map (3:) (subs [])) 
 ++ map (1:) (subs [] ++ map (3:) (subs []) ++ map (2:) (subs [] ++ map (3:) (subs []))) ->
[[]] ++ [[3]] ++ [[2]] ++ map (2:) (map (3:) [[]]) = [2:3:[]] = [[2,3]] ++ [[1]] ++ [[1,3]] ++ [[1,2]] ++ [[1,2,3]] ...
-}

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)
-- interleave 1 [2,3,4] -> [[1,2,3,4], [2,1,3,4], [2,3,1,4], [2,3,4,1]]

perms :: [a] -> [[a]]
perms []        = [[]]
perms (x:xs)    = concat (map (interleave x) (perms xs))
-- list all possible orderings

choices :: [a] -> [[a]]
choices = concat . map perms . subs
-- list all possible choices in a list

solutions :: Expr -> [Int] -> Int -> Bool
solutions e ns n = elem (values e) (choices ns) && eval e == [n]
{-
elem is to check a item is in a Foldable or not, <- element
to check the list of Val is in choices, and the solution is equals to [n]
-}

split :: [a] -> [([a], [a])]
split []        = []
split [_]       = []
split (x:xs)    = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]
-- to find all posible combination to split a list into two non-empty list

exprs :: [Int] -> [Expr]
exprs []    = []
exprs [n]   = [Val n]
exprs ns    = [e | (ls,rs) <- split ns,
                   l       <- exprs ls,
                   r       <- exprs rs,
                   e       <- combine l r]
-- list all possible Expr

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]
-- combine (Val 1) (Val 2) -> [1+2,1-2,1*2,1/2]

ops :: [Op]
ops = [Add,Sub,Mul,Div]

solutions_n :: [Int] -> Int -> [Expr]
solutions_n ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]
-- list all posible solutions

type Result = (Expr,Int)

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n,n) | n > 0] -- only +ve N is valid
results ns  = [res | (ls,rs) <- split ns,
                     lx      <- results ls,
                     ry      <- results rs,
                     res     <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y] -- check if valid

solutions_n2 :: [Int] -> Int -> [Expr]
solutions_n2 ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]

{-
as the following properties persent, further optimization can be done
x+y = y+x
x*y = y*x
x*1 = 1*x = x
x/1 = x
-}

valid_n :: Op -> Int -> Int -> Bool
valid_n Add x y = x <= y
valid_n Sub x y = x > y
valid_n Mul x y = x /= 1 && y /= 1 && x <= y
valid_n Div x y = y /= 1 && mod x y == 0

results' :: [Int] -> [Result]
results' []  = []
results' [n] = [(Val n,n) | n > 0]
results' ns  = [res | (ls,rs) <- split ns,
                      lx      <- results' ls,
                      ry      <- results' rs,
                      res     <- combine'' lx ry]

combine'' :: Result -> Result -> [Result]
combine'' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid_n o x y]

solutions_n3 :: [Int] -> Int -> [Expr]
solutions_n3 ns n = [e | ns' <- choices ns, (e,m) <- results' ns', m == n]

main :: IO ()
main = print $ length $ solutions_n3 [1,3,7,10,25,50] 100