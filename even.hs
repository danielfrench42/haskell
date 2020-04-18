evn :: Int -> Bool
evn 0 = True
evn 1 = False
evn x = evn (x-2)