data Tree a
  = Leaf a | Branch (Tree a) (Tree a)
  
data N = Z | S N deriving Show
  
plus :: N -> N -> N
plus Z y = y
plus (S x) y = S (plus x y)

maxN :: N -> N -> N
maxN Z y = y
maxN x Z = x
maxN (S x) (S y) = S (maxN x y)
  
fold :: (a -> b) -> (b -> b -> b) -> (Tree a -> b)

fold leaf branch = \ t -> case t of
  Leaf x -> leaf x
  Branch l r -> branch (fold leaf branch l) (fold leaf branch r)
  
summe_leaves = fold (\ x -> S Z) plus (Branch (Branch (Leaf Z) (Leaf Z)) (Branch (Branch (Leaf Z) (Leaf Z)) (Leaf Z)))

summe_branches = fold (\ x -> Z) (\ x y -> S (plus x y)) (Branch (Leaf Z) (Leaf Z))
 
summe_keys = fold (\ x -> x) plus (Branch (Leaf Z) (Leaf Z))

depth = fold (\ x -> Z) (\ x y -> S (maxN x y)) (Branch (Branch (Leaf Z) (Leaf Z)) (Branch (Branch (Leaf Z) (Leaf Z)) (Leaf Z)))

biggest_key = fold (\ x -> x) (\ x y -> maxN x y) (Branch (Branch (Leaf (S Z)) (Leaf Z)) (Branch (Branch (Leaf Z) (Leaf Z)) (Leaf Z)))

bottomleft_key = fold (\ x -> x) (\ x y -> x) (Branch (Branch (Branch (Leaf (S Z)) (Leaf Z)) (Leaf Z)) (Branch (Branch (Leaf Z) (Leaf Z)) (Leaf Z)))
