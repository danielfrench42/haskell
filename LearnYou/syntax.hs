addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors a b = (fst a + fst b, snd a + snd b)

third :: (a,b,c) -> c
third (_, _, z) = z