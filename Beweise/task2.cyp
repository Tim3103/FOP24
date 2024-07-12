data N = Z | S N deriving Show

plus :: N -> N -> N
plus x y = case x of Z -> y ; S x' -> S (plus x' y)

Lemma plus_assoc : forall a :: N, b :: N, c :: N :
  plus a (plus b c) .=. plus (plus a b) c
Proof by induction on a :: N
Case Z
  Show : plus Z (plus b c) .=. plus (plus Z b) c
  Proof ... QED
Case S a'
  Fix a' :: N
  Assume IV : 
    plus a' (plus b c).=. plus (plus a' b) c
  Then Show : 
    plus(S a')(plus b c) .=. plus (plus (S a') b) c
  Proof ... QED
QED
