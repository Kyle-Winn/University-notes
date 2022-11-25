module Tutorial10 where

import Test.QuickCheck
import Control.Monad
import Data.Char

-- Question 1

-- 1a

ok :: String -> Bool
ok s = and[ (isLower x) | x <- s] && ((length s) < 6)

-- 1b

f :: [String] -> String
f xs = if (length (helper xs) == 0) then "zzzzz" else minimum(helper xs)
      where
      helper xs = [x | x <- xs, ok x]

-- 1c

g :: [String] -> String
g []     = "zzzzz"
g (x:xs) | (ok x)    = minimum(x : [g xs])
         | otherwise = minimum([g xs])


-- 1d

h :: [String] -> String
h xs =  foldr min "zzzzz" (filter ok xs)
      

-- Question 2

-- 2a

i :: [a] -> [a] -> [a]
i s1 s2 = tail s1 ++ [head s2]

-- 2b
--call i on an element at c, and element at index c + 1; if c > length-1, c = 0
j :: [[a]] -> [[a]]
j xs = [ i (xs !! c) (xs !! (helper xs c)) | (x, c) <- zip xs [0..]]
      where
      helper xs c = if (c + 1) > ((length xs)-1) then 0 else c + 1

-- 2c



k :: [[a]] -> [[a]]
k xs = go xs
   where 
    go []       = []
    go [a]     = [i a (head xs)]
    go (y:ys)   = [i y (head ys)] ++ go ys

-- Question 3

data Prop = X
          | Y
          | T
          | F
          | Not Prop
          | Prop :&&: Prop
          | Prop :||: Prop
          | Prop :->: Prop
  deriving (Eq, Show)

instance Arbitrary Prop where
  arbitrary = sized gen
    where
    gen 0 =
      oneof [ return X,
              return Y,
              return T,
              return F ]
    gen n | n>0 =
      oneof [ return X,
              return Y,
              return T,
              return F,
              liftM Not prop,
              liftM2 (:&&:) prop prop,
              liftM2 (:||:) prop prop,
              liftM2 (:->:) prop prop]
      where
      prop = gen (n `div` 2)

-- 3a

eval :: Bool -> Bool -> Prop -> Bool
eval x y (p :&&: q)     = (eval x y p) && (eval x y q)
eval x y (p :||: q)     = (eval x y p) || (eval x y q)
eval x y (p :->: q)     = not (eval x y p) || (eval x y q)
eval x y (Not p)        = not (eval x y p)
eval x y X              = x
eval x y Y              = y

-- 3b

simple :: Prop -> Bool
simple T          = True
simple F          = True
simple p          = helper p
    where 
    helper (p :&&: q) = (helper p) && (helper q)
    helper (p :||: q) = (helper p) && (helper q)
    helper (p :->: q) = (helper p) && (helper q)
    helper (Not p)    = (helper p)
    helper T          = False
    helper F          = False 
    helper X          = True
    helper Y          = True



-- 3c

simplify :: Prop -> Prop
simplify T       = T
simplify F       = F
simplify X       = X
simplify Y       = Y
simplify (Not T) = F
simplify (Not F) = T
simplify (F :&&: p) = F
simplify (p :&&: F) = F
simplify (T :&&: p) = simplify p
simplify (p :&&: T) = simplify p
simplify (T :||: p) = T
simplify (p :||: T) = T
simplify (F :||: p) = simplify p
simplify (p :||: F) = simplify p
simplify (F :->: p) = T 
simplify (p :->: T) = T
simplify (T :->: p) = simplify p
simplify (p :->: F) = (Not (simplify p))
simplify (p :->: q) =  simplify(( simplify p) :->: (  simplify q)) --remove the outer simplify to make it work for abstract variables.
simplify (p :||: q) =  ((simplify p) :||: (simplify q))
simplify (p :&&: q) =  ((simplify p) :&&: (simplify q))

