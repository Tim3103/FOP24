import qualified Prelude

data Bool = False | True deriving Prelude.Show

-- not
not :: Bool -> Bool
not x = case x of
  False -> True
  True -> False

-- antivalenz
xor :: Bool -> Bool -> Bool
xor x y = case x of
  False -> y
  True -> not y

-- implication
impl :: Bool -> Bool -> Bool
impl x y = case x of
  False -> True
  True -> y

-- majority
major :: Bool -> Bool -> Bool -> Bool
major x y z = case x of
  False -> case y of
    False -> False
    True -> z
  True -> case y of
    False -> z
    True -> True

-- majority without case 
mand :: Bool -> Bool -> Bool
mand x y = case x of
  False -> False
  True -> y

mor :: Bool -> Bool -> Bool -> Bool
mor x y z = case x of
  False -> case y of
    False -> z
    True -> True
  True -> True

major1 :: Bool -> Bool -> Bool -> Bool
major1 x y z = mor (mand x y) (mand x z) (mand y z)






{-
major2 :: Bool -> Bool -> Bool -> Bool
major2 x y z = if (x == True && y == True) then True
                else if (y == True && z == True) then True
                        else if (x == True && z == True) then True
                                else False
-}
