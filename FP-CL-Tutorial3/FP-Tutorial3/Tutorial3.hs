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
lookUp = undefined

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec = undefined

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp = undefined

-- 2.
encipher :: Int -> Char -> Char
encipher = undefined

-- 3.
normalise :: String -> String
normalise = undefined

normaliseRec :: String -> String
normaliseRec = undefined

prop_normalise :: String -> Bool
prop_normalise = undefined

-- 4.
enciphers :: Int -> String -> String
enciphers = undefined

-- 5.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey = undefined

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec = undefined

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey = undefined

-- 6.
decipher :: Int -> Char -> Char
decipher = undefined

decipherStr :: Int -> String -> String
decipherStr = undefined


-- ** Optional Material

-- 7.
candidates :: String -> [(Int, String)]
candidates = undefined


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
encrypt = undefined

-- 9.
decrypt :: Int -> String -> String
decrypt = undefined
