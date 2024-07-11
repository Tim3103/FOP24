data N = Z | S N deriving Show

plus :: N -> N -> N
plus x y = case x of Z -> y ; S x' -> S (plus x' y)

times :: N -> N -> N
times x y = case x of {Z -> Z; S x' -> plus y (times x' y);}
