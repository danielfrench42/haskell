import Prelude hiding (map)
keepTwos :: [Int] -> [Int]
keepTwos []                 = []
keepTwos (x:xs) | x==2      = 2:keepTwos xs
                | otherwise = keepTwos xs

--countTwos :: [Int] -> Int
--countTwos []                 = 0
--countTwos (x:xs) | x==2      = 1+countTwos xs
--                | otherwise = countTwos xs

countTwos :: [Int] -> Int
--countTwos xs = length (keepTwos xs)
countTwos = length . keepTwos

sum' :: [Int] -> Int
sum' = foldr (+) 0

map :: (a -> b) -> [a] -> [b]
map f []        = []
map f (x:xs) = f x:map f xs

--map f . map g = map (f . g)

th :: [[a]] -> [a]
th = tail.head

revmap :: (a -> b) -> [a] -> [b]
revmap f []     = []
revmap f (x:xs) = revmap f xs ++ [f x]

--revmap :: (a → b) → [a] → [b]
--revmap f = reverse . map f

last' :: [a] -> a
last' [x]       = x
last' (_:xs) = last' xs

a y [] = []
a y (x:xs)  | x>y       = x:a y xs
            | otherwise = a y xs


--Day 2


member :: Int -> [Int] -> Bool
member _ [] = False
--member x (y:ys) = if x==y then True else member x ys

--member x (y:ys) | x==y
--                | True 
--                | otherwise member x ys

member x (y:ys) = x==y || member x ys
--map (`elem` [3,5,7]) [1..10]

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x l@(y:ys) | x<=y = x:l
                  | otherwise = y:insert x ys

type Bag a = [(a,Int)]

ins :: Eq a => a -> Bag a -> Bag a
ins = undefined

bag :: Eq a => [a] -> Bag a
bag _ []      = []
bag (x:xs)  = ins x (bag xs)
--bag (x:xs)  = x `ins` (bag xs)
--bag = foldr ins []
--x :: a
-- xs :: [a]
-- Want: Bag a


plus :: Int -> Int -> Int
plus x y = x+y
--map (plus 3) [1..10]


nest = map (:[])

positive' :: Int -> Bool
positive' = (>=0)

data Nat    = Zero
            | Succ Nat