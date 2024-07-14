data List a = Nil | Cons a (List a)

append :: List a -> List a -> List a
append Nil ys = ys
append ( Cons x xss ) ys = Cons x (append xss ys)

reverse :: List a -> List a
reverse Nil = Nil
reverse (Cons x xs) = append (reverse xs) (Cons x Nil)

axiom E : forall xs :: List a,  ys :: List a: reverse (append xs ys) .=. append (reverse ys) (reverse xs)

Lemma : forall xs :: List a: reverse (reverse xs) .=. xs
	Proof by induction on xs :: List a
	Case Nil
		Show : reverse (reverse Nil) .=. Nil
	Proof by rewriting reverse (reverse Nil)
		(by def reverse) .=. reverse Nil
		(by def reverse) .=. Nil
	QED
	Case Cons x xs'
	Fix x :: a, xs' :: List a
	Assume IV:
		reverse (reverse xs') .=. xs'
	Then Show:
		reverse (reverse (Cons x xs')) .=. Cons x xs'
	Proof by rewriting reverse (reverse (Cons x xs'))
		(by def reverse) .=. reverse (append (reverse xs') (Cons x Nil))
		(by E) .=. append (reverse (Cons x Nil)) (reverse (reverse xs'))
		(by def reverse) .=. append (append (reverse Nil) (Cons x Nil)) (reverse (reverse xs'))
		(by def reverse) .=. append (append Nil (Cons x Nil)) (reverse (reverse xs'))
		(by def append) .=. append (Cons x Nil) (reverse (reverse xs'))
		(by def append) .=. Cons x (append Nil (reverse (reverse xs')))
		(by def append) .=. Cons x (reverse (reverse xs'))
		(by IV) .=. Cons x xs'
	QED
QED