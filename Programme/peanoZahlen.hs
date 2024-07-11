data N = Z | S N deriving Show

plus :: N -> N -> N
plus x y = case x of Z -> y ; S x' -> S (plus x' y)