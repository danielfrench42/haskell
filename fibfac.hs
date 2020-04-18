fac :: Int -> Int
fac x =
    if x<2
        then 1
    else x * fac (x-1)

fib :: Int -> Int
fib x = 
    if x==1
        then 0
    else if x==2
        then 1
    else fib (x-1) + fib (x-2) 

fibAlt :: Int -> Int
fibAlt 1 = 0
fibAlt 2 = 1
fibAlt x = fib (x-1) + fib (x-2)



fac' :: Int -> Int
fac' x  | x > 1     = fac'(x - 1) * x
        | otherwise = 1