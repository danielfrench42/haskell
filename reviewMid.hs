data Expr = Zero
         | Succ Expr
         | Sum [Expr]
         | IfPos Expr Expr
         deriving Show

sem :: Expr -> Int
sem Zero          = 0
sem (Succ a)      = 1 + sem a
-- sem (Sum lst)     = sum (map sem lst)
sem (Sum [])      = 0
sem (Sum (x:xs))  = sem x + sem (Sum xs)
sem (IfPos a b)   | (sem a) > 0 = sem a
                  | otherwise = sem b

type Pos = (Int,Int)
data Move = JumpTo Pos
          | UpBy Int
          | Right'
          | Seq Move Move

sem' :: Move -> Pos -> Pos
sem' (JumpTo (a,b)) (_,_) = (a,b) 
sem' (UpBy y') (x,y)      = (x,y')
sem' (Right') (x,y)     = (x+1,y)
sem' (Seq m1 m2) (x,y)    = (fst a,snd b)
                           where a = sem' m1 (x,y)
                                 b = sem' m2 (x,y)