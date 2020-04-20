moduel SentSyn where


data Sentence = Phrase Noun Verb Noun
                | And Sentence Sentence
                deriving Show

data Noun = Dogs | Teeth deriving Show
data Verb = Have deriving Show


s1 :: Sentence
s1 = Phrase Dogs Have Teeth
s2 :: Sentence
s2 = Phrase Teeth Have Dogs
s3 :: Sentence
s3 = And s1 s2
dpgs s1 `And` Dogs

err2 = Phrase Dogs have