data N = Z | S N deriving Show

plus :: N -> N -> N
plus x y = case x of Z -> y ; S x' -> S (plus x' y)

times :: N -> N -> N
times x y = case x of {Z -> Z; S x' -> plus y (times x' y);}

mini :: N -> N -> N
mini Z Z = Z
mini Z (S y) = Z
mini (S x) Z = Z
mini (S x) (S y) = S (mini x y)

maxi :: N -> N -> N
maxi Z Z = Z
maxi Z (S y) = (S y)
maxi (S x) Z = (S x)
maxi (S x) (S y) = S (maxi x y)

minus :: N -> N -> N
minus Z b = Z ; minus a Z = a 
minus (S a') (S b') = minus a' b'
