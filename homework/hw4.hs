type Prog = [Cmd]

data Cmd = LD Int 
         | ADD 
         | MULT 
         | DUP 
         | INC 
         | SWAP 
         | POP Int 

type Rank    = Int
type CmdRank = (Int,Int)

rankC :: Cmd -> CmdRank
rankC (LD _)   = (0,1)
rankC ADD      = (2,1)
rankC MULT     = (2,1)
rankC DUP      = (1,1)
rankC INC      = (1,1)
rankC (POP k)  = (k,0)

-- rankP :: Prog -> Maybe Rank

-- rank :: Prog -> Rank -> Maybe Rank

data Shape = X
           | TD Shape Shape
           | LR Shape Shape
           deriving Show

type BBox = (Int,Int)

-- bbox :: Shape -> BBox

-- rect :: Shape -> Maybe BBox