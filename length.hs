import Prelude hiding (length,sum,head,tail)

length :: [a] -> Int
length []       = 0
length (_:xs)   = 1 + length xs

length' x   | not (null x)  = length (tail x) + 1
            | otherwise     = 0

sum :: [Int] -> Int
sum []      = 0
sum (x:xs)  = x + sum xs

head :: [a] -> a
head (x:_) = x

tail :: [a] -> [a]
tail (_:xs) = xs

g [] ys = ys
g (x:xs) ys = g xs (x:ys)
