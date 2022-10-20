-- Informatics 1 - Functional Programming 
-- Class Test 2022

module ClassExam where

import Data.Char
import Test.QuickCheck

-- Problem 1

-- a

f :: String -> Int
f xs = sum[ ord x | x <- xs, isAlpha x ]

-- b

g :: String -> Int
g [] = 0
g (x:xs) 
    | isAlpha x = ord x + g xs 
    | otherwise =  g xs 

-- c

h :: String -> Int
h xs = sum(map (ord) (filter (isAlpha) xs ))

-- d

prop_fgh :: String -> Bool
prop_fgh xs = (f xs == g xs) && (g xs == h xs)
-- +++ OK, passed 100 tests.


-- Problem 2

-- a

c :: String -> String -> Bool
c xs ys = and[x == y | (x,y) <- zip xs ys, isAlpha x && isAlpha y]

-- b

d :: String -> String -> Bool
d [] _ = True
d _ [] = True
d (x:xs) (y:ys)
    | (isAlpha x && isAlpha y) = x == y && d xs ys
    | otherwise = d xs ys

-- c

prop_cd :: String -> String -> Bool
prop_cd xs ys = c xs ys == d xs ys
-- +++ OK, passed 100 tests.