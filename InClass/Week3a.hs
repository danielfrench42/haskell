import Data.List (sort)

ignore :: a -> String
ignore x = "Relax"

--ones :: [Int]
--ones = 1:ones   --Inifinite List

morse :: [Int]
--morse = Cons 1 (Cons 0 morse)
morse = 1:0:morse

take' :: Int -> [Int] -> [Int]
take' 0 []       = []
take' a []       = []
take' a (x:xs)   = x:take (a-1) xs

data Grade = A | B | C | D | F
           deriving (Show,Eq,Ord,Enum)  --shows data type values as data type,equality symbol, order, [1..9]

--instance Show Grade where
--    show A = "1.0"
--    show B = "2.0"
--    show C = "3.0"
--    show D = "4.0"
--    show F = "5.0"

pass :: Grade -> Bool
pass    = (<D)
-- pass A  = True
-- pass B  = True
-- pass C  = True
-- pass _  = False
--pass D  = False
--pass F  = False


data Tree   = Node Int Tree Tree
            | Leaf
            deriving Eq

instance Show Tree where
    show Leaf = "_"
    show (Node x Leaf Leaf) = show x
    show (Node x l r) = show x++"{"++show l++","++show r++"}"

--l = Node 3 (Node 1 Leaf Leaf) (Node 5 Leaf Leaf)
--t = Node 6 l (Node 9 Leaf (Node 8 Leaf Leaf))

nd :: Int -> Tree
nd i = Node i Leaf Leaf

l = Node 3 (nd 1) (nd 5)
t = Node 6 l (Node 9 Leaf (nd 8))

find :: Int -> Tree -> Bool
find _ Leaf = False
find i (Node j l r) = i==j  || find i l || find i r
-- find i (Node j l r) | i==j  = True
--                     | i<j   = find i l
--                     | otherwise = find i r

minT :: Tree -> Int
minT (Node x Leaf _)    = x
minT (Node _ l _)       = minT l

type Pt = (Int,Int)
data Geo    = 
              Rect Int Int
            | Square Int
            | Point Int Int
            | Line Pt Pt
            deriving (Eq, Show)

instance Ord Geo where
    g <= h = size g <= size h

r = Rect 3 4
p = Point 5 7

pict :: [Geo]
pict    = [r,p,scale 3 r, Square 3]

scale :: Int -> Geo -> Geo
scale f (Rect w h)  = Rect (f*w) (f*h)
scale f (Square s) = Square (f*s)
scale _ p = p

width :: Geo -> Int
width (Rect w _)    = w
width (Square s)    = s
width _             = 0

smaller :: Geo -> Geo -> Bool
smaller g h = size g < size h


size :: Geo -> Int
size (Rect a b) = a*b
size (Square a) = a*a
size (Point _ _) = 0