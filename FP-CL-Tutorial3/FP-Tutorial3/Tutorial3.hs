module Tutorial3 where

import Data.Char
import Data.List
import Test.QuickCheck


-- These are some helper functions for makeKey and makeKey itself.
-- Exercises continue below.

rotate :: Int -> [Char] -> [Char]
rotate k list | 0 <= k && k <= length list = drop k list ++ take k list
              | otherwise = error "Argument to rotate too large or too small"

--  prop_rotate rotates a list of lenght l first an arbitrary number m times,
--  and then rotates it l-m times; together (m + l - m = l) it rotates it all
--  the way round, back to the original list
--
--  to avoid errors with 'rotate', m should be between 0 and l; to get m
--  from a random number k we use k `mod` l (but then l can't be 0,
--  since you can't divide by 0)

prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

alphabet = ['A'..'Z']

makeKey :: Int -> [(Char, Char)]
makeKey k = zip alphabet (rotate k alphabet)

-- ** Caesar Cipher Exercises

-- 1.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp x xs = if null [ snd y | y <- xs, fst y == x ] then x else ([ snd y | y <- xs, fst y == x ] !! 0)

lookUpRec :: Char -> [(Char, Char)] -> Char 
lookUpRec chr [] = chr 
lookUpRec chr (x:xs)
      | chr == fst x = snd x
      | otherwise = lookUpRec chr xs

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp x xs = (lookUpRec x xs) == (lookUp x xs)
-- +++ OK, passed 100 tests.

-- 2.
encipher :: Int -> Char -> Char
encipher x char = lookUp char (makeKey x)

-- 3.
normalise :: String -> String
normalise xs = [ toUpper x | x <- xs, isAlpha x ]

normaliseRec :: String -> String
normaliseRec [] = []
normaliseRec (x:xs)
      | isAlpha x = toUpper x : normaliseRec xs 
      | otherwise = normaliseRec xs 

prop_normalise :: String -> Bool
prop_normalise ws = normalise ws == normaliseRec ws 
-- +++ OK, passed 100 tests.

-- 4.

enciphers :: Int -> String -> String
enciphers x ws =  [ encipher x y  | y <- normalise ws]

-- 5.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey xs = [ (snd x, fst x) | x <- xs]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec (x:xs)
      | otherwise = (snd x, fst x) : reverseKeyRec xs

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey xs = reverseKey xs == reverseKeyRec xs
-- +++ OK, passed 100 tests.


-- 6.

decipher :: Int -> Char -> Char
decipher x char = head [snd y | y <- reverseKey (makeKey x), char == fst y]  


decipherStr :: Int -> String -> String
decipherStr x ws = [ decipher x y | y <- ws, isAlpha y && isUpper y]


-- ** Optional Material

-- 7.
-- for every integer taken from [1..26] decipher string by it

--decipher string with every integer up to 26, if and is inside deciphered string return a tuple with key and text
candidates :: String -> [(Int, String)]
candidates ws = [ (snd y, fst y) | y <- [(decipherStr x ws, x) | x <- [1..26]], isInfixOf "AND" (fst y) || isInfixOf "THE" (fst y)]

splitEachFive :: String -> [String]
splitEachFive xs | length xs > 5 = take 5 xs : splitEachFive (drop 5 xs)
                 | otherwise     = [ fillToFive xs ]

fillToFive :: String -> String
fillToFive xs = xs ++ replicate (5 - length xs) 'X'

-- An alternative solution demonstrating 'repeat'
fillToFive' :: String -> String
fillToFive' xs = take 5 (xs ++ repeat 'X')

-- The following example shows why 'transpose' is not
--  invertible in general. The transpose function
--  takes the 'columns' of a list of lists, and makes
--  them the 'rows' of a new list of lists. 
--
-- [[o n e],           [[o t t f f],       [[o n e e e],
--  [t w o],            [n w h o i],        [t w o r],  
--  [t h r e e],   -->  [e o r u v],   -->  [t h r e],  
--  [f o u r],          [e r e],            [f o u], 
--  [f i v e]   ]       [e],        ]       [f i v]     ]   

-- 8.
encrypt :: Int -> String -> String
encrypt x ws =  intercalate "" (transpose (splitEachFive(enciphers x ws)))

-- 9.

split  :: String -> Int -> [String]
split [] y = []
split xs y | otherwise = take y xs : split (drop y xs) y
     

--split into segments of (string divided by 5), transpose, intercalate, and finally decipher
decrypt :: Int -> String -> String
decrypt x ws = decipherStr x (intercalate "" (transpose (split (ws) ((length ws) `div` 5))))
