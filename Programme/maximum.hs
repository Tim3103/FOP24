data N = Z | S N deriving Show
data List a = Nil | Cons a (List a) deriving Show

maxiPeano :: N -> N -> N
maxiPeano Z Z = Z
maxiPeano Z (S y) = (S y)
maxiPeano (S x) Z = (S x)
maxiPeano (S x) (S y) = S (maxiPeano x y)

maxiList :: List N -> N
maxiList xs = case xs of
  Nil        -> Z
  Cons x xs' -> maxiPeano x (maxiList xs')
