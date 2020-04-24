--Daniel French
-- Homework 2

data Circuit = Both Gates Links
     -- deriving Show


data Gates = A IntA GateFn Gates
               | O IntO GateFn Gates 
               | X IntX GateFn Gates 
               | N IntN GateFn Gates
               | None

type IntA = Int
type IntO = Int
type IntX = Int
type IntN = Int


data GateFn = And | Or | Xor | Not
          deriving Show

data Links = To One Two Three Four Links
               | None'

type One = Int
type Two = Int
type Three = Int
type Four = Int

instance Show Circuit where
     show (Both gate1 link1) = show gate1++show link1

instance Show Gates where
     show (A num gate gateF2) = show num++":"++show gate++";\n"++show gateF2  
     show (O num gate gateF2) = show num++":"++show gate++";\n"++show gateF2
     show (X num gate gateF2) = show num++":"++show gate++";\n"++show gateF2
     show (N num gate gateF2) = show num++":"++show gate++";\n"++show gateF2
     show (None) = ""


-- instance Show GateFn where

instance Show Links where
     show (To a b c d link2) = "from "++show a++"."++show b++" to "++show c++"."++show d++";\n"++show link2
     show None' = ""




-- VARIABLES FOR TESTING

circ :: Circuit
circ = Both gate1 link1 

-- gate :: Gates
-- gate =

gate1 :: Gates
gate1 = A 1 And gate2

gate2 :: Gates
gate2 = O 2 Or gate3

gate3 :: Gates
gate3 = None

link1 :: Links
link1 = To 1 2 3 4 link2

link2 :: Links
link2 = None'