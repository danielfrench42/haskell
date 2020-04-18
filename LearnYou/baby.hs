doubleMe :: Int -> Int
doubleMe x = x + x

doubleUs :: Int -> Int -> Int
doubleUs x y = x*2 + y*2

doubleSmallNumber :: Int -> Int
doubleSmallNumber x = if x > 100
                    then x
                    else x*2


boomBangs xs = [if x < 10
                then "BOOM!"
                else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

triangle = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10], a^2 + b^2 == c^2, a+b+c == 24]