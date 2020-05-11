type Prog = [Cmd]
data Cmd = LD Int
      | ADD
      | MULT
      | DUP
      deriving Show
type Stack = [Int]
         --deriving Show
type D = [Int] -> [Int]

-- sem :: Prog -> D
-- sem [] = (\ s -> s) --Do nothing so the stack can be given input arguments
-- --sem (c:cs) = (\ s -> semCmd c (sem cs s)) --Got it backwards
-- sem (c:cs) = (\ s -> sem cs (semCmd c s)) --There we go

-- semCmd :: Cmd -> D
-- semCmd (LD i) = (\ (s) -> (i:s))
-- semCmd (ADD) =  (\ (a:b:s) -> (a+b:s))
-- semCmd (MULT) = (\ (a:b:s) -> (a*b:s))
-- semCmd (DUP) = (\ (a:s) -> (a:a:s))

-- in1 = [LD 3, DUP, ADD, DUP, MULT]
-- in2 = [LD 3, ADD]
-- in3 = []

semCmd :: Cmd -> Maybe Stack -> Maybe Stack
semCmd (LD i) (Just s)          = Just (i:s)
semCmd DUP    (Just vs@(v:_))   = Just (v:vs)
semCmd ADD    (Just (v1:v2:vs)) = Just (v1+v2:vs)
semCmd MULT   (Just (v1:v2:vs)) = Just (v1*v2:vs)
semCmd _      _                 = Nothing

sem :: Prog -> Maybe Stack -> Maybe Stack:
sem []     s = s
sem (c:cs) s = sem cs (semCmd c s)

tst1 = [LD 3, DUP, ADD, DUP, MULT]
tst2 = []::Prog
err1 = [LD 3, ADD]
err2 = [LD 3, MULT]
err3 = [DUP]
tests  = [tst1,      tst2,    err1,    err2,    err3]
expect = [Just [36], Just [], Nothing, Nothing, Nothing]
test = map (\p->sem p (Just [])) tests == expect



--Exercise 2
data Cmd' = Pen Mode
         | MoveTo Int Int
         | Seq Cmd' Cmd'

data Mode = Up | Down

type State = (Mode,Int,Int)

type Line = (Int,Int,Int,Int)
type Lines = [Line]



semS :: Cmd' -> State -> (State,Lines)
semS (Pen Down)   (Down,x,y)  = ((Down,x,y),[])
semS (Pen Down)   (Up,x,y)    = ((Down,x,y),[])
semS (Pen Up)     (Down,x,y)  = ((Up,x,y),[])
semS (Pen Up)     (Up,x,y)    = ((Up,x,y),[])
semS (MoveTo x y) (Up,a,b)    = ((Up,x,y),[])
semS (MoveTo x y) (Down,a,b)  = ((Down,x,y),[(a,b,x,y)])
semS (Seq c m)    state       = (secState,combine lineOne lineTwo)
                                 where  (secState,lineTwo) = semS m state
                                        (firState,lineOne) = semS c secState
            
combine :: Lines -> Lines -> Lines
combine a b = a++b 
                          

sem' :: Cmd' -> Lines
sem' x = snd(semS x (Up,0,0))

shape :: Lines
shape = [(2,2,4,4),(4,4,7,4),(7,4,7,2),(7,2,2,2)]

c :: Cmd'
c = Seq one two

one :: Cmd'
one = MoveTo 4 7

two :: Cmd'
two = Pen Down
