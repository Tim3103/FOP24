data T = F T T | C deriving Show

size :: T -> Prelude.Int
size x = case x of
  C -> 1
  F left right -> 1 Prelude.+ size left Prelude.+ size right

depth :: T -> Prelude.Int
depth x = case x of 
  C -> 0
  F left right -> 1 + max (depth left) (depth right) 