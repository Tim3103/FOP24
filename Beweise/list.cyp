data List a = Nil | Cons a (List a)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs') ys = Cons x (append xs' ys)

reverse :: List a -> List a
reverse Nil = Nil
reverse (Cons x xs') = append (reverse xs') (Cons x Nil)
     
axiom E : forall xs :: List a, ys :: List a, zs :: List a : append xs (append ys zs) .=. append (append xs ys) zs

Lemma list : forall xs :: List a, ys :: List a : reverse (append xs ys)
  .=. append (reverse ys) (reverse xs)
Proof by induction on xs :: List a
Case Nil
  Show : reverse (append Nil ys)
  .=. append (reverse ys) (reverse Nil)
  Proof by rewriting append (reverse ys) (reverse Nil)
      (by def reverse) .=. append (reverse ys) Nil
      (by def append) .=. append (reverse ys) (append Nil Nil)
      (by def reverse) .=. append (reverse ys) (append Nil (reverse Nil))
      (by def append) .=. append (reverse ys) (reverse Nil)
    QED
Case (Cons x xs')
  Fix x :: a, xs' :: List a
  Assume IV : 
    reverse (append xs' ys) .=. append (reverse ys) (reverse xs')
  Then Show : 
    reverse (append (Cons x xs') ys) .=. append (reverse ys) (reverse (Cons x xs'))
  Proof by rewriting reverse (append (Cons x xs') ys)
      (by def append) .=. reverse (Cons x (append xs' ys))
      (by def reverse) .=. append (reverse (append xs' ys)) (Cons x Nil)
      (by def IV) .=. append (append (reverse ys) (reverse xs')) (Cons x Nil)
      (by def E) .=. append (reverse ys) (append (reverse xs') (Cons x Nil))
      (by def reverse) .=. append (reverse ys) (reverse (Cons x xs'))
    QED
QED

