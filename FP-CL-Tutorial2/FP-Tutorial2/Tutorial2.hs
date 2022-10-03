module Tutorial2 where

import Data.Char
import Data.List
import Test.QuickCheck


-- 1. inRange

inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, x >= lo && x <= hi]


-- 2. multDigits

multDigits :: String -> Int
multDigits str = product[digitToInt x | x <- str, isDigit x]

countDigits :: String -> Int
countDigits str = length[x | x <- str, isDigit x]

prop_multDigits :: String -> Bool
prop_multDigits str = (multDigits str) <= 9 ^ (countDigits str)
--(quickCheck prop_multDigits) passes 100 tests successfully

-- 3. capitalise and title

capitalise :: String -> String
capitalise word = [(toUpper (head word))] ++ [ toLower x | x <- (tail word)]

title :: [String] -> [String]
title words = [ capitalise (head words)] ++ [ if (length x) >= 4 then (capitalise x) else ([ toLower y | y <- x]) | x <- (tail words) ]




-- 4. score and totalScore

score :: Char -> Int
score x  = if (isAlpha x) then (length (['z'] ++[ y | y <- ['a', 'e', 'i', 'o', 'u'], (toLower x) == y] ++ [ y | y <- ['A' .. 'Z'], x == y] )) else 0

totalScore :: String -> Int
totalScore xs = product[ score x | x <- xs, isAlpha x]

--(quickCheck prop_totalScore_pos) passes 100 tests successfully.
prop_totalScore_pos :: String -> Bool
prop_totalScore_pos xs = (totalScore xs) >= 1


-- ** Optional Material

-- 5. crosswordFind

crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words = [ x | x <- words, (length x) == len , (elemIndex letter x) == Just pos]

-- 6. search

--zip[0..] "abcde"
--if letter i is equal to the target letter, return the index and put it in a list
search :: String -> Char -> [Int]
search str goal = [ x | (x, i) <- zip [0..] str, i == goal ]

-- Depending on the property you want to test, you might want to change the type signature
prop_search :: String -> Char -> Bool
prop_search str goal =  product (search str goal) >= 1

