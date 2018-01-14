import Data.List

votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)
-- Eg count "Red" votes = 2

-- remove duplicates
rmdups :: Eq a => [a]-> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs) -- /= x means != x
-- if want to use folding, Y-combinator must be used

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result


ballots = [["Red", "Green"],
           ["Blue"],
           ["Green", "Red", "Blue"],
           ["Blue", "Green", "Red"],
           ["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))
-- exclude x in list

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head
-- map head -> ["Red", "Blue", "Green", "Blue", "Green"]
-- result -> [(1,"Red"),(2,"Blue"),(2,"Green")]
-- map snd -> ["Red", "Blue", "Green"]

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
                [c]   -> c
                (c:_) -> winner' (elim c bs)