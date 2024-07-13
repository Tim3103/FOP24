data List a = Nil | Cons a (List a) deriving Show

append xs ys = case xs of
     Nil        -> ys
     Cons x xs' -> Cons x (append xs' ys)

revers xs = case xs of
     Nil        -> Nil
     Cons x xs' -> append (revers xs') (Cons x Nil)
