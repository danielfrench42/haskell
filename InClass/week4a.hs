data Cond = T | Not Cond
          deriving Show
data Stmt = While Cond Stmt
          | Noop
          deriving Show

--data Ints = One Int | Add Int Ints

--data Ints = One Int | Join Ints Ints

--type Ints = [Int] --not acceptable

-- prettyPrint :: 
-- prettyPrint d       = 

ppCond :: Cond -> String
ppCond T       ="T"
ppCond (Not c) = "not("++ppCond c++")"

indent :: Int -> String
indent i = take (4*i) (repeat ' ')

-- data Str = Seq Chr Str | Empty
-- data Chr = A | B | Chr