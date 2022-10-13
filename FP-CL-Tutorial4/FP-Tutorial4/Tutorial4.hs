module Tutorial4 where

import Data.Char
import Data.List
import Test.QuickCheck


-- ** Optional material

-- 1. doubles
-- a.
doublesComp :: [Int] -> [Int]
doublesComp xs =  [ (x * 2) | x <- xs]

-- b.
doublesRec :: [Int] -> [Int]
doublesRec [] = []
doublesRec (x:xs) = x * 2 : doublesRec xs

-- c.
doublesHO :: [Int] -> [Int]
doublesHO xs =  map (*2) xs

-- d.
prop_doubles :: [Int] -> Bool
prop_doubles xs = if doublesRec xs == doublesComp xs then doublesComp xs == doublesHO xs else False

-- 2. aboves
-- a.
abovesComp :: Int -> [Int] -> [Int]
abovesComp x xs=  [ y | y <- xs, y > x]

-- b.
abovesRec :: Int -> [Int] -> [Int]
abovesRec _ [] = []
abovesRec x (y:ys) 
    | y > x = y : abovesRec x ys
    | otherwise = abovesRec x ys


-- c.
abovesHO :: Int -> [Int] -> [Int]
abovesHO x xs =  filter (>x) xs

-- d.
prop_aboves :: Int -> [Int] -> Bool
prop_aboves x xs =  if abovesComp x xs == abovesRec x xs then abovesRec x xs == abovesHO x xs else False


-- 3. parity
-- a.
xor :: Bool -> Bool -> Bool
xor x y = (not x) == y

-- b.
parityRec :: [Bool] -> Bool
parityRec [] = True
parityRec (x:xs) = x `xor` parityRec xs

-- c.
parityHO :: [Bool] -> Bool
parityHO xs =  foldr (xor) True xs 

-- d.
prop_parity :: [Bool] -> Bool
prop_parity xs =  parityHO xs == parityRec xs

-- 4. allcaps
-- a.
allcapsComp :: String -> Bool
allcapsComp xs =  and[ isUpper x | x <- xs, isAlpha x]

-- b.
allcapsRec :: String -> Bool
allcapsRec [] = True
allcapsRec (x:xs) 
    | isAlpha x = isUpper x && allcapsRec xs
    | otherwise = allcapsRec xs

-- c.
allcapsHO :: String -> Bool
allcapsHO xs =  foldr (&&) True (map isUpper (filter isAlpha xs))

-- d.
prop_allcaps :: String -> Bool
prop_allcaps xs =  if allcapsComp xs == allcapsHO xs then allcapsHO xs == allcapsRec xs else False


-- ** Optional material
-- Matrix manipulation

type Matrix = [[Rational]]

-- 5
-- a.

-- :type all returns Foldable t => (a -> Bool) -> t a -> Bool
uniform :: [Int] -> Bool
uniform xs = all (== head xs) xs

-- b.
--1. every list in the matrix is of equal length
--2. there is at least one row and one column in the list of lists

valid :: Matrix -> Bool
valid xs = uniform (map (length) xs ) && ((length xs >= 1) && (length (head xs) >= 1))


-- 6.
width :: Matrix -> Int
width m = if valid m then length (head m) else error "invalid matrix"

height :: Matrix -> Int
height m = if valid m then length m else error "invalid matrix"

plusRow :: [Rational] -> [Rational] -> [Rational]
plusRow xs ys = zipWith (+) xs ys

plusM :: Matrix -> Matrix -> Matrix
plusM x y = if  (height x == height y ) && (width x == width y) then [ plusRow c i | (c,i) <- zip x y] else error "Failed to add"

-- 7.
dot :: [Rational] -> [Rational] -> Rational
dot x y = if length x == length y then sum(zipWith (*) x y) else error "cannot compute; wrong size matrices"

timesM :: Matrix -> Matrix -> Matrix
timesM xs ys = if width xs == height ys then [map (dot x) (transpose ys) | x <- xs] else error "cannot compute; wrong size matrices"

-- ** Challenge

-- 8.
-- Mapping functions
mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f = undefined

zipMatrix :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrix f = undefined

-- All ways of deleting a single element from a list
removes :: [a] -> [[a]]     
removes = undefined

-- Produce a matrix of minors from a given matrix
minors :: Matrix -> [[Matrix]]
minors m = undefined

-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Int -> Int -> Matrix
signMatrix w h = undefined
        
determinant :: Matrix -> Rational
determinant = undefined

cofactors :: Matrix -> Matrix
cofactors m = undefined        
                
scaleMatrix :: Rational -> Matrix -> Matrix
scaleMatrix k = undefined

inverse :: Matrix -> Matrix
inverse m = undefined

-- Tests
identity :: Int -> Matrix
identity n = undefined

prop_inverse2 :: Rational -> Rational -> Rational 
                -> Rational -> Property
prop_inverse2 a b c d = undefined

type Triple a = (a,a,a)
        
prop_inverse3 :: Triple Rational -> 
                 Triple Rational -> 
                 Triple Rational ->
                 Property
prop_inverse3 r1 r2 r3 = undefined
