data T = T Bool Bool Bool deriving Show 

instance Eq T where T b1 b2 b3 == T b1' b2' b3' = (b1 == b1') && (b2 == b2') && (b3 == b3')

t1 = T False False False
t2 = T False False False