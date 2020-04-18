import Prelude hiding (not)

plus :: Int -> Int -> Int
plus x y = x+y

plus' :: (Int,Int) -> Int
plus' (x,y) = x+y

not :: Bool -> Bool
not x =
    if x==True
        then False
    else True

sign :: Int -> Int
sign x  | x>0 = 1
        | x<0 = -1
        | True = 0