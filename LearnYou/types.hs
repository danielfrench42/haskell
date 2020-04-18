--removeNonUppercase :: String -> String
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase string = [out | out <- string, out `elem` ['A'..'Z']]


addThree :: (Integral a) => a -> a -> a -> a
--addThree :: Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Int -> Int
factorial input = product [1..input]

x:y:z:zs = [1,2,3,4,5]