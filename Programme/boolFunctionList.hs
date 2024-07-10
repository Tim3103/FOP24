data List = Nil | Cons Bool List deriving Prelude.Show

and1 :: List -> Bool
and1 l = case l of 
  Nil -> True
  Cons x xs -> case x of 
    False -> False
    True -> and1 xs
