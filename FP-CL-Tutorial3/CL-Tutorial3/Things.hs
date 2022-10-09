import Data.List


data Thing = A | B | C | D | E deriving (Eq,Show)

things :: [Thing]
things = [ A, B, C, D, E ]

data Colour = Amber | Blue deriving Eq

colour :: Thing -> Colour
colour A = Amber
colour B = Amber
colour C = Amber
colour D = Blue
colour E = Amber
            
data Shape = Square | Disc deriving Eq

shape :: Thing -> Shape
shape A = Square
shape B = Square
shape C = Disc
shape D = Square
shape E = Square
           
data Size = Big | Small deriving Eq

size :: Thing -> Size
size A = Big
size B = Big
size C = Big
size D = Big
size E = Small
       
data Border = Thin | Thick deriving Eq

border :: Thing -> Border
border A = Thick
border B = Thin
border C = Thick
border D = Thick
border E = Thick

type Predicate u = u -> Bool

isAmber :: Predicate Thing
isAmber x = colour x == Amber

isBlue :: Predicate Thing
isBlue x = colour x == Blue

isSquare :: Predicate Thing
isSquare x = shape x == Square

isDisc :: Predicate Thing
isDisc x = shape x == Disc

isBig :: Predicate Thing
isBig x = size x == Big

isSmall :: Predicate Thing
isSmall x = size x == Small

hasThinBorder :: Predicate Thing
hasThinBorder x = border x == Thin

hasThickBorder :: Predicate Thing
hasThickBorder x = border x == Thick

--------------------------------------
-- Your task is to replace 'undefined' with the definitions of the operators below.

(|=) :: Predicate Thing -> Predicate Thing -> Bool
a |= b = and [ b x | x <- things, a x]

(|/=) :: Predicate Thing -> Predicate Thing -> Bool
a |/= b = not (or[ b x | x <- things, a x])

checkAlla :: [Predicate Thing] -> Thing -> [Bool]
checkAlla [] y = []
checkAlla (x:xs) y = x y : checkAlla xs y

(||=) :: [Predicate Thing] -> Predicate Thing -> Bool
a ||= b =  and [b x | x <- things, and (checkAlla a x)]
--[isBlue, isSquare] ||= isBig
--everything that is blue and a square is also big
--True

--not([isBig, isAmber] ||= isDisc)
--returns True (something that is big and is amber is not a disc)

--excercise 7

neg :: Predicate u -> Predicate u
(neg a) x = not (a x)

(|:|) :: Predicate u -> Predicate u -> Predicate u
(a |:| b) x = a  x || b x

(&:&) :: Predicate u -> Predicate u -> Predicate u
(a &:& b) x = a x && b x

--1. isBig &:& isAmber |= isDisc == False
--2. isBig &:& isDisc |= isAmber == True
--3. isSmall &:& neg isBlue |= neg isDisc == True
--4. isBig |:| isAmber |= neg isSquare == False
--5. neg (isSquare |:| isBlue) |= hasThickBorder == True
--6. neg isSquare &:& neg isAmber |= isDisc == True

--2, 3, 5 and 6 are return true


-- Combining our infix operators may lead to ambiguous expressions. For example, 'a |:| b &:& c' can be read in two ways: either '(a |:| b) &:& c' or 'a |:| (b &:& c)'. We can always instruct Haskell which one to choose by adding paranthesis ourselves. 
-- But there are also ways of instructing Haskell how to disambiguate. The next lines are setting the precedence for our infix operators. To learn more about this, watch the video on Precedence from the CL week 3 list of videos (https://media.ed.ac.uk/media/1_25gncs98). It is not crucial you understand all this at the moment; we will remind you of fixity declarations later on during the course. In short, these lines guarantee a non-ambiguous parsing of our code.
infixr 0 |=
infixr 0 |/=
infixr 0 ||=
infixr 2 |:|
infixr 3 &:&
