--Daniel French
--Homework 2

data Circuit = Both Gates Links

data Gates = A IntA GateFn Gates
               | None


data GateFn = And | Or | Xor | Not

data Links = To One Two Three Four Links
               | None'

type IntA = Int
type One = Int
type Two = Int
type Three = Int
type Four = Int

instance Show Circuit where
     show (Both gate1 link1) = show gate1++show link1

instance Show Gates where
     show (A num gate gateF2) = show num++":"++show gate++";\n"++show gateF2 
     show (None) = ""


instance Show GateFn where
     show (And) = "and"
     show (Or) = "or"
     show (Xor) = "xor"
     show (Not) = "not"

instance Show Links where
     show (To a b c d e) = "from "++show a++"."++show b++" to "++show c++"."++show d++";\n"++show e
     show None' = ""



-- VARIABLES FOR TESTING

circ :: Circuit
circ = Both gate1 link1 

gate1 :: Gates
gate1 = A 1 And gate2

gate2 :: Gates
gate2 = A 2 Or gate3

gate3 :: Gates
gate3 = None

gateF1 :: GateFn
gateF1 = And

link1 :: Links
link1 = To 1 1 2 1 link2

link2 :: Links
link2 = To 1 2 2 2 link3

link3 :: Links
link3 = None'