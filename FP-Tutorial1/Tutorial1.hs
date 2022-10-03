module Tutorial1 where

import PicturesSVG -- needed for the optional chess part
import Test.QuickCheck

-- 2.
double :: Int -> Int
double x = x + x

--typing double 21 returns 42
--typing type double returns Int->Int
--double 21 has type Integer
--double "three" returns couldnt match type Char with Int; expected an integer, got a string


square :: Int -> Int
square x = x * x


-- 3.
isTriple :: Int -> Int -> Int -> Bool
isTriple a b c =  square a + square b == square c

-- 4.
leg1 :: Int -> Int -> Int
leg1 x y = square x - square y

leg2 :: Int -> Int -> Int
leg2 x y = 2 * x * y

hyp :: Int -> Int -> Int
hyp x y = square x + square y

-- 5.
prop_triple :: Int -> Int -> Bool
prop_triple x y = isTriple (leg1 x y) (leg2 x y) (hyp x y)

--5.a) takes in two integers x and y and returns a bool based on if it generated a triple or not
--b) expected true as the formula generates triples for all positive integers x and y
--c) quickCheck worked.


-- 7.
pic1 :: Picture
pic1 = above (beside knight (invert knight)) (beside (invert knight) knight)

pic2 :: Picture
pic2 = above (beside knight (invert knight)) (flipV (beside knight (invert knight)))

-- ** Functions

twoBeside :: Picture -> Picture
twoBeside x = beside x (invert x)

-- 8.
twoAbove :: Picture -> Picture
twoAbove x = above x (invert x)

fourPictures :: Picture -> Picture
fourPictures x = twoBeside (twoAbove x) 

-- 9.
-- a)
emptyRow :: Picture
emptyRow = repeatH 4 (beside whiteSquare blackSquare)

--emptyRow = repeatH 4 (twoBeside whiteSquare)

-- b)
otherEmptyRow :: Picture
otherEmptyRow = repeatH 4 (beside blackSquare whiteSquare)

-- c)
middleBoard :: Picture
middleBoard = (repeatV 2 (above emptyRow otherEmptyRow))

-- d)
whiteRow :: Picture
whiteRow = (over (beside rook (beside knight (beside bishop (beside queen (beside king (beside bishop (beside knight rook))))))) otherEmptyRow)

blackRow :: Picture
blackRow = (over (beside (invert rook) (beside (invert knight) (beside (invert bishop) (beside (invert queen) (beside (invert king) (beside (invert bishop) (beside (invert knight) (invert rook)))))))) emptyRow)


whitePawns :: Picture
whitePawns = (over (repeatH 8 pawn) emptyRow)

blackPawns :: Picture
blackPawns = (over (repeatH 8 (invert pawn)) otherEmptyRow)


-- e)
populatedBoard :: Picture
populatedBoard = above (above blackRow blackPawns) (above (above middleBoard whitePawns) whiteRow)
