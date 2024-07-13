data List a = Nil | Cons a (List a) deriving Show

app :: forall e . List e -> List e -> List e
app xs ys = case xs of
  Nil -> ys
  Cons x xs' -> Cons x (app xs' ys)

data Bin a = Leaf | Branch (Bin a) a (Bin a) deriving Show

pre :: forall a . Bin a -> List a
pre t = case t of
  Leaf -> Nil
  Branch l k r -> Cons k (app (pre l) (pre r))

-- Bsp für: pre (Branch (Branch Leaf False Leaf) True (Branch (Branch Leaf True Leaf) False Leaf))

-- Cons True (app (pre (Branch Leaf False Leaf)) (pre (Branch (Branch Leaf True Leaf) False Leaf))
-- Cons True (app (Cons False (app (pre (Leaf)) (pre (Leaf)))) (Cons False (app (pre (Branch Leaf True Leaf)) (pre (Leaf))))
-- Cons True (app (Cons False (app (Nil) (Nil))) (Cons False (app (pre (Branch Leaf True Leaf)) (Nil)))
-- Cons True (app (Cons False (app (Nil) (Nil))) (Cons False (app (Cons True (app (pre Leaf) (pre Leaf))) (Nil)))
-- Cons True (app (Cons False (app (Nil) (Nil))) (Cons False (app (Cons True (app (Nil) (Nil))) (Nil)))
-- Cons True (app (Cons False Nil) (Cons False (app (Cons True Nil) (Nil)))
-- Cons True (app (Cons False Nil) (Cons False (Cons True (app Nil Nil)))
-- Cons True (app (Cons False Nil) (Cons False (Cons True Nil))
-- Cons True (Cons False (app Nil (Cons False (Cons True Nil)))
-- Cons True (Cons False (Cons False (Cons True Nil)))

inord :: forall a . Bin a -> List a
inord t = case t of
  Leaf -> Nil
  Branch l k r -> app (app (inord l) (Cons k Nil)) (inord r)
  
-- Bsp für: inord (Branch (Branch Leaf 2 Leaf) 4 (Branch Leaf 3 Leaf))

-- app (app (inord (Branch Leaf 2 Leaf) (Cons 4 Nil)) (inord (Branch Leaf 3 Leaf))
-- app (app (app (app (inord Leaf) (Cons 2 Nil)) (inord Leaf)) (Cons 4 Nil)) (app (app (inord Leaf) (Cons 3 Nil)) (inord Leaf)))
-- app (app (app (app Nil (Cons 2 Nil)) Nil) (Cons 4 Nil)) (app (app Nil (Cons 3 Nil)) Nil))
-- app (app (app (Cons 2 Nil) Nil) (Cons 4 Nil)) (app (Cons 3 Nil) Nil))
-- app (app (Cons 2 (app Nil Nil) (Cons 4 Nil)) (Cons 3 (app Nil Nil))
-- app (app (Cons 2 Nil) (Cons 4 Nil)) (Cons 3 Nil)
-- app (Cons 2 (app Nil (Cons 4 Nil))) (Cons 3 Nil)
-- app (Cons 2 (Cons 4 Nil)) (Cons 3 Nil)
-- Cons 2 (app (Cons 4 Nil) (Cons 3 Nil))
-- Cons 2 (Cons 4 (app Nil (Cons 3 Nil)))
-- Cons 2 (Cons 4 (Cons 3 Nil))

