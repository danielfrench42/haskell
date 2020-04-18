--Daniel French

import Data.List (nub,sort)

norm :: Ord a => [a] -> [a]
norm = sort . nub

type Bag a = [(a,Int)]

box :: Bag Int
box = [(2,3),(1,4),(4,2)]
boxtwo = [(2,5),(1,6),(4,4)]

ins :: Eq a => a -> Bag a -> Bag a
ins a []            = [(a,1)]
ins a ((x,n):xs)    | a==x = ((x,n+1):xs)                        
                    | otherwise = (x,n):ins a xs

del :: Eq a => a -> Bag a -> Bag a
del a []            = []
del a ((x,n):xs)    | (a == x && n >= 2) = ((x,n-1):xs)       
                    | (a == x && n == 1) = xs
                    | otherwise = (x,n):del a xs


bag :: Eq a => [a] -> Bag a
bag [] = []                                                   
bag (x:xs) = ins x (bag xs)

auxBag :: Eq a => Bag a -> Bag a -> Bool
auxBag [] ((y,b):ys)                                 = True
auxBag [] []                                         = True
auxBag ((x,n):xs) ((y,b):ys)    | (x == y && n <= b) = auxBag xs ys
                                | (x /= y)           = auxBag ((x,n):xs) ys
                                | (x == y && n > b)  = False
                                | otherwise          = True

sortBag :: Ord a => Bag a -> Bag a
sortBag ((x,n):xs)     = sort ((x,n):xs)

subbag :: Ord a => Bag a -> Bag a -> Bool
subbag [] ((y,b):ys)            = True
subbag ((x,n):xs) []            = False                  
subbag ((x,n):xs) ((y,b):ys)    = auxBag (sortBag ((x,n):xs)) (sortBag ((y,b):ys))
 

auxIsBag :: Eq a => Bag a -> Bag a -> Bag a
auxIsBag [] _                   = []
auxIsBag _ []                   = []
auxIsBag ((x,n):xs) ((y,b):ys)  | (x == y && n <= b) = (x,n):auxIsBag xs ys
                                | (x /= y)           = auxIsBag ((x,n):xs) ys
                                | (x == y && n > b)  = (y,b):auxIsBag xs ys

isbag :: Ord a => Bag a -> Bag a -> Bag a
isbag [] _                      = []
isbag _ []                      = []
isbag ((x,n):xs) ((y,b):ys)     = auxIsBag (sortBag((x,n):xs)) (sortBag((y,b):ys))


size :: Bag a -> Int
size []         = 0                                           
size ((x,n):xs) = n + size xs


type Node = Int
type Edge = (Node,Node)
type Graph = [Edge]
type Path = [Node]

g :: Graph
g = [(1,2),(1,3),(2,3),(2,4),(3,4)]

h :: Graph 
h = [(1,2),(1,3),(2,1),(3,2),(4,4)]

nodes :: Graph -> [Node]
nodes []            = []                                     
nodes ((n,e):xs)    = norm (n:nodes xs)

suc :: Node -> Graph -> [Node]
suc a []            = []                              
suc a ((x,n):xs)    | (a==x) = n:suc a xs
                    | otherwise = suc a xs      

detach :: Node -> Graph -> Graph
detach _ []         = []
detach a ((b,c):xs) |(a==b) = detach a xs
                    |(a==c) = detach a xs                     
                    |otherwise = (b,c):detach a xs

auxCyc :: Int -> Graph
auxCyc x        | (x-1) > 1 = (x-1,x):auxCyc(x-1)
                | otherwise = (x-1,x):[]

cyc :: Int -> Graph
cyc 0       = []
cyc 1       = [(1,1)]
cyc x       = reverse((x,1):auxCyc x)

type Number = Int
type Point = (Number,Number)
type Length = Number
data Shape = Pt Point
            | Circle Point Length
            | Rect Point Length Length
            deriving Show
type Figure = [Shape]
type BBox = (Point,Point)

f = [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]

width :: Shape -> Length
width (Pt _)        = 0
width (Circle _ w)  = (2*w)                     
width (Rect _ w _)    = w

bbox :: Shape -> BBox
bbox (Pt a)                 = (a,a)
bbox (Circle (p,t) a)       = ((p-a,t-a),(p+a,t+a))    
bbox (Rect (p,t) w l)       = ((p,t),(p+w,t+l))

minX :: Shape -> Number
minX (Pt (x,y))                         = x
minX (Circle (x,y) l)   | (x-l) < (x+l) = (x-l)     
                        | otherwise     = (x+l)
minX (Rect (x,y) _ _)                   = x 

addPt :: Point -> Point -> Point
addPt (x,y) (a,b) = (x+a,y+b)

move :: Shape -> Point -> Shape
move (Pt (x,y)) (a,b)             = Pt (addPt (x,y) (a,b))  
move (Circle (x,y) l) (a,b)       = Circle ((x,y) `addPt` (a,b)) l 
move (Rect (x,y) l w) (a,b)       = Rect ((x,y) `addPt` (a,b)) l w

moveTo :: Number -> Shape -> Shape
moveTo a (Pt (x,y))         = Pt (a,y)  
moveTo a (Circle (x,y) l)   = Circle (x-((minX (Circle (x,y) l))-a),y) l 
moveTo a (Rect (x,y) l w)   = Rect (a,y) l w 

alignLeft :: Figure -> Figure
alignLeft [Pt (a,b), Circle (c,d) p, Rect (e,f) l w]    | minX (Pt (a,b)) < minX (Circle (c,d) p) && minX (Pt (a,b)) < minX (Rect (e,f) l w) = 
                                                            [Pt (a,b), moveTo a (Circle (c,d) p), moveTo a (Rect (e,f) l w)]
                                                        |minX (Circle (c,d) p) < minX (Pt (a,b)) && minX (Circle (c,d) p) < minX (Rect (e,f) l w) =    
                                                            [moveTo (c-p) (Pt (a,b)), Circle (c,d) p, moveTo (c-p) (Rect (e,f) l w)]
                                                        |minX (Rect (e,f) l w) < minX (Circle (c,d) p) && minX (Rect (e,f) l w) < minX (Pt (a,b)) = 
                                                            [moveTo e (Pt (a,b)), moveTo e (Circle (c,d) p), Rect (e,f) l w]


dist :: Point -> Point -> Float
dist (a,b) (c,d)    = sqrt( ( ((fromIntegral(d)) - (fromIntegral(b)))^2 ) + ( ((fromIntegral(c)) - (fromIntegral(a)))^2 ))

checkBbox :: Point -> BBox -> Bool
checkBbox (x,y) ((a,b),(c,d))  | (x >= a && y >= b && x <= c && y <= d) = True
                                    | otherwise = False

chBbox :: BBox -> BBox -> Bool
chBbox ((a,b),(c,d)) ((e,f),(g,h))  | (a >= e && a <= g && b >= f && b <= h && c >= e && c <= g && d >= f && d <= h) = True
                                    | otherwise = False

inside :: Shape -> Shape -> Bool
inside (Pt (x,y)) (Pt (a,b))                    | (x==a && y==b)    = True 
                                                | otherwise         = False
inside (Pt (x,y)) (Circle (a,b) l)              | ((dist (x,y) (a,b)) <= fromIntegral(l)) = True
                                                | otherwise         = False
inside (Pt (x,y)) (Rect (a,b) w l)              = checkBbox (x,y) (bbox (Rect (a,b) w l)) 
inside _ (Pt (x,y))                             = False
inside (Circle (x,y) r1) (Circle (a,b) r2)      | (x==a && y==b && r1 <= r2) = True
                                                | otherwise = False                                                            
inside (Circle (x,y) r1) (Rect (a,b) w l)       = chBbox (bbox(Circle (x,y) r1)) (bbox(Rect (a,b) w l))
inside (Rect (x,y) w1 l1) (Rect (a,b) w2 l2)    | (x >= a && y >= b && (x+w1) <= (a+w2) && (y+l1) <= (b+l2)) = True
                                                | otherwise = False
inside (Rect (x,y) w1 l1) (Circle (a,b) r)      = chBbox (bbox(Rect (x,y) w1 l1)) (bbox(Circle (a,b) r))