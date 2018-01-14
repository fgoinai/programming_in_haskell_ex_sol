-- by book
data Nat = Zero | Succ Nat
instance Show Nat where
    show Zero = "Zero"
    show (Succ n) = "(Succ " ++ show n ++ ")"

-- by book
add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

-- ex 1
mult :: Nat -> Nat -> Nat
mult n Zero = Zero
mult m (Succ n) = add m $ mult m n

-- data Ordering = LT | EQ | GT
-- compare :: Ord a => a -> a -> Ordering

-- from books
data List a = Nil | Cons a (List a)
data Tree a = Leaf a | Node (Tree a) a (Tree a)

-- ex2
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = compare x y == EQ
occurs x (Node l y r) = case compare x y of
                        LT -> occurs x l
                        EQ -> True
                        GT -> occurs x r

-- ex3
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)
instance Show a => Show (Tree' a) where
    show (Leaf' y) = "Leaf' " ++ (show y)
    show (Node' l r) = "Node' (" ++ show l ++ ") (" ++ show r ++ ")" 

leavesCount :: Tree' a -> Int
leavesCount (Leaf' _) = 1
leavesCount (Node' l r) = foldl (+) 1 (map leavesCount [l,r])

balanced :: Tree' a -> Bool
balanced (Leaf' _) = True
balanced (Node' l r) = abs (leavesCount l - leavesCount r) <= 1 && foldl (&&) True (map balanced [l,r])
-- balanced (Node' l r) = abs (leavesCount l - leavesCount r) <= 1 && balanced l && balanced r

-- ex4
balance :: [a] -> Tree' a
balance xs | length xs == 1 = Leaf' (xs!!0)
           | otherwise      = Node' ((balance . fst . cutInHalf) xs) ((balance . snd . cutInHalf) xs)

cutInHalf :: [a] -> ([a], [a])
cutInHalf xs = (take (half xs) xs, drop (half xs) xs)
    where
        half ys = floor $ (/) ((fromIntegral . length) ys) 2

-- ex5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val a) = f a
folde f g (Add a b) = g (folde f g a) (folde f g b)

-- ex6
eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (\_ -> 1) (+)

-- ex 7
-- data Mb a = J a | N

-- instance Eq a => Eq (Mb a) where
--     J a == J b = a == b
--     J _ == N   = False
--     N == J _   = False
--     N == N     = True

-- instance Eq a => Eq [a] where
--     [] == [] = True
--     _ == [] = False
--     a == b | length a /= length b = False
--            | foldl (&&) True ([fst y == snd y | y <- zip a b]) == False = False
--            | otherwise = True

-- ex8

-- from book
type Assoc k v = [(k, v)]
type Pair a = (a, a)
type Subst = Assoc Char Bool
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop -- Intersect: T->T->T else F
          | Imply Prop Prop -- T -> F -> F, else T
          | Or Prop Prop --disjunction, F->F->F else T
          | Equal Prop Prop -- a == b -> T else F

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not $ eval s p
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q -- True >= False, fml...
eval s (Or p q)    = eval s p || eval s q
eval s (Equal p q) = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q
vars (Equal p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
    where bss = bools (n-1)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
    where vs = rmdups (vars p)
    
isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

-- ex 9
data Expr9 = Val9 Int | Add9 Expr9 Expr9 | Multi9 Expr9 Expr9
type Cont = [Op]
data Op = EVALADD Expr9 | ADD Int | EVALMULTI Expr9 | MULTI Int

eval9 :: Expr9 -> Cont -> Int
eval9 (Val9 n) c = exec c n
eval9 (Add9 x y) c = eval9 x (EVALADD y : c)
eval9 (Multi9 x y) c = eval9 x (EVALMULTI y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVALADD y : c) n = eval9 y (ADD n : c)
exec (ADD n : c) m = exec c (n+m)
exec (EVALMULTI y : c) n = eval9 y (MULTI n : c)
exec (MULTI n : c) m = exec c (n*m)

value9 :: Expr9 -> Int
value9 e = eval9 e []