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
