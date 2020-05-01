type Prog = [Cmd]

data Cmd = LD Int
          | ADD
          | MULT
          | DUP

type Stack = [Int]

-- sem :: Prog -> D
-- sem (x:xs) = x:[]

-- semCmd :: Cmd -> D
-- semCmd ADD = 0
