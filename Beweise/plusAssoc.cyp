data N = Z | S N

plus :: N -> N -> N
plus  Z     y = y 
plus (S x') y = S (plus x' y)

Lemma plus_assoc : forall a :: N, b :: N, c :: N :
  plus a (plus b c) .=. plus (plus a b) c
Proof by induction on a :: N
Case Z
  Show : plus Z (plus b c) .=. plus (plus Z b) c
  Proof by rewriting plus Z (plus b c)
    (by def plus) .=. plus b c
    (by def plus) .=. plus (plus Z b) c
    QED
Case S a'
  Fix a' :: N
  Assume IV : 
    plus a' (plus b c).=. plus (plus a' b) c
  Then Show : 
    plus(S a')(plus b c) .=. plus (plus (S a') b) c
  Proof by rewriting plus (S a') (plus b c)
    (by def plus) .=. S (plus a' (plus b c))
    (by IV) .=. S (plus (plus a' b) c)
    (by def plus) .=. plus (S (plus a' b)) c
    (by def plus) .=. plus (plus (S a') b) c
    QED
QED
