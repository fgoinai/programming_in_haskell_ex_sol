module Ch12.Content where

-- inc :: [Int] -> [Int]
-- inc [] = []
-- inc (x:xs) = x+1 : inc xs

-- sqr :: [Int] -> [Int]
-- sqr [] = []
-- sqr (x:xs) = x^2 : sqr xs

-- inc = map (+1)
-- sqr = map (^2)

-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b

-- instance Functor [] where
--     -- fmap :: (a -> b) -> [a] -> [b]
--     fmap = map

-- data Maybe a = Nothing | Just a

-- instance Functor Maybe where
--     -- fmap :: (a -> b) -> Maybe a -> Maybe b
--     fmap _ Nothing = Nothing
--     fmap g (Just x) = Just (g x)

data Expr = Val Int | Div Expr Expr

eval :: Expr -> Maybe Int
eval (Val n)   = Just n
eval (Div x y) = eval x >>== \n ->
    eval y >>== \m ->
    safediv n m

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just $ div n m

(>>==) :: Maybe a -> (a -> Maybe b) -> Maybe b
mx >>== f = case mx of
    Nothing -> Nothing
    Just x -> f x