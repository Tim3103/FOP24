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
