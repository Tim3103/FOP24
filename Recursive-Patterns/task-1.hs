fold :: a -> a -> (Bool -> a)
fold false true = \ t -> case t of
  False -> false
  True -> true
  
fold1 :: b -> (a -> b) -> (Maybe a -> b)
fold1 nothing just = \ t -> case t of
  Nothing -> nothing
  Just x -> just x

data Pair a b = P a b
fold2 :: (a -> b -> c) -> (Pair a b -> c)
fold2 p = \ t -> case t of
  P x y -> p x y

fold3 :: (a -> c) -> (b -> c) -> Prelude.Either a b -> c
fold3 left right = \ t -> case t of
  Left x -> left x
  Right x -> right x

