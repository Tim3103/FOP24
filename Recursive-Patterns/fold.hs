data List a = Nil | Cons a (List a) deriving Show

head1 :: List a -> a
head1 (Cons x xs) = x

fold :: r -> (a -> r -> r) -> List a -> r
fold nil cons = \xs -> case xs of
  Nil -> nil
  Cons x xs' -> cons x (fold nil cons xs')

tails = fold (Cons Nil Nil) (\x y -> Cons (Cons x (head1 y)) y)
