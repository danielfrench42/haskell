-- --HW2 for CS381. Group 14: Hunter Land, Emre Dogan, Daniel French

-- --1a
-- data Cmd = Pen Mode
--      	| Moveto Pos Pos
--      	| Def String Pars Cmd --String - Name, Pars - Params/Arguments, Cmd - Command
--      	| Call String Vals
--      	| Pair Cmd Cmd
--      	deriving Show

-- data Mode = Up
--       	| Down
--       	deriving Show
-- data Pos = N Int
--      	| S String
--      	deriving Show
-- type Pars = [String]
-- type Vals = [Int]

-- --1b

-- --1c
-- stepsR :: Int -> Cmd
-- stepsR 0 = Pair (Moveto (N 0) (N 0)) (Pen Up)
-- stepsR i = Pair
--            	(Pair
--                  	(Moveto (N i) (N i))
--                  	(Moveto (N (i-1)) (N i)))
--            	(stepsR (i-1))

-- steps :: Int -> Cmd
-- steps i = Pair
--            	(Pair
--                 	(Moveto (N i) (N i))
--                 	(Pen Down))
--            	(stepsR i)




--2a
data Circuit = Both Gates Links

data Gates = A IntA GateFn Gates
               | None

type IntA = Int

data GateFn = And | Or | Xor | Not

data Links = To One Two Three Four Links
               | None'

type One = Int
type Two = Int
type Three = Int
type Four = Int

--2c
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
     show (To a b c d link2) = "from "++show a++"."++show b++" to "++show c++"."++show d++";\n"++show link2
     show None' = ""

halfAdder = Both gate1 link1

-- VARIABLES FOR TESTING

circ :: Circuit
circ = Both (A 1 gateF1 gate2) (To 1 1 2 1 link2) 

gate1 :: Gates
gate1 = A 1 gateF1 gate2

gate2 :: Gates
gate2 = A 2 gateF2 gate3

gate3 :: Gates
gate3 = None

gateF1 :: GateFn
gateF1 = Xor

gateF2 :: GateFn
gateF2 = And

link1 :: Links
link1 = To 1 1 2 1 link2

link2 :: Links
link2 = To 1 2 2 2 link3

link3 :: Links
link3 = None'
