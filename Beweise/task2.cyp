data N = Z | S N

plus :: N -> N -> N
plus Z y = y
plus (S x') y = S (plus x' y)

plus' :: N -> N -> N
plus' Z y = y
plus' (S x) y = plus' x (S y)


Lemma neben: forall x :: N, y :: N : plus x (S y) .=. plus (S x) y
Proof by induction on x :: N
    Case Z
        Show: plus Z (S y) .=. plus (S Z) y
        Proof by rewriting plus Z (S y)
        (by def plus) .=. S y
        (by def plus) .=. S (plus Z y)
        (by def plus) .=. plus (S Z) y
        QED
     Case S x'
        Fix x' :: N
        Assume 
          IV: plus x' (S y) .=. plus (S x') y
        Then
        Show: plus (S x') (S y) .=. plus (S(S x')) y
        Proof by rewriting plus (S x') (S y)
        (by def plus) .=. S (plus x' (S y))
        (by IV) .=. S (plus (S x') y)
        (by def plus) .=. plus (S (S x')) y
        QED             
QED

Lemma haupt: forall x :: N, y :: N : plus x y  .=. plus' x y
Proof by induction on x::N generalizing y::N
 Case Z
  For fixed y::N
  Show: plus Z y .=. plus' Z y
  Proof by rewriting plus Z y
   (by def plus) .=. y
   (by def plus') .=. plus' Z y
  QED
 Case S x'
  Fix x' :: N
   Assume
    IV: forall y::N :  plus x' y  .=. plus' x' y
  Then
  For fixed y :: N
  Show: plus (S x') y .=. plus' (S x') y
    Proof by rewriting plus (S x') y
        (by neben) .=. plus x' (S y)
        (by IV) .=. plus' x' (S y)
        (by def plus') .=. plus' (S x') y
   QED
QED
