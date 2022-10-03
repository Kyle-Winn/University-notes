--Kyle Winn


--Excercise 1
--A; as it is the only one that isn't different to the others in some way

--Excercise 2
data Thing = A | B | C | D | E  deriving (Eq, Show)
things :: [Thing]
things = [A, B, C, D, E]

data Border = Thick | Thin deriving (Eq, Show)
data Colour = Orange | Blue deriving (Eq, Show)
data Shape = Square | Circle deriving (Eq, Show)
data Size = Big | Small deriving (Eq, Show)

colour :: Thing -> Colour
colour x = if isOrange x then Orange else Blue

shape :: Thing -> Shape
shape x = if isSquare x then Square else Circle

border :: Thing -> Border
border x = if hasBorder x then Thick else Thin

size :: Thing -> Size
size x = if isBig x then Big else Small

--Excercise 3
type Predicate u = u -> Bool

isOrange :: Predicate Thing
isOrange x = x `elem` [A, B, C, E]

isBlue :: Predicate Thing
isBlue x = not (isOrange x)

isSquare :: Predicate Thing
isSquare x = x `elem` [A, B, D, E]

isCircle :: Predicate Thing
isCircle x = not (isSquare x)

hasBorder :: Predicate Thing
hasBorder x = x `elem` [A, C, D, E]

noBorder :: Predicate Thing
noBorder x = not (hasBorder x)

isBig :: Predicate Thing
isBig x = x `elem` [A, B, C, D]

isSmall :: Predicate Thing
isSmall x = not (isBig x)

--Excercise 4
--Every blue square has a thin border
-- and [not (hasBorder x) | x <- things, isSquare x && not (isOrange x)]
--returns false, as there are no blue squares with a thin border

--Some amber disc is not big
--or [ not (isBig x) | x <- things, not (isSquare x) && isOrange x]
--returns false, as there are no amber discs that are small.

--Excercise 5
--"Every X (square) is not Y (blue)"
--and [ isOrange x | x <- things, isSquare x]

--"It is not the case that some X (square) is Y (blue)"
--not (or [ not (isOrange x) | x <- things, isSquare x])

--Excercise 6
thingsOtherThan :: Thing -> [Thing]
thingsOtherThan y = [ x | x <- things, x /= y]

properties :: [Predicate Thing]
properties = [isOrange, isBlue, isBig, isSmall, hasBorder, noBorder, isSquare, isCircle]

propertiesOf :: Thing -> [Predicate Thing]
propertiesOf y = [x | x <- properties, x y ]

isPropertyOfAnotherThing :: Predicate Thing -> Thing -> Bool
isPropertyOfAnotherThing p x = or [ p y | y <- (thingsOtherThan x)]

propertiesOnlyOf :: Thing -> [Predicate Thing]
propertiesOnlyOf x = [ y | y <- (propertiesOf x), not (isPropertyOfAnotherThing y x) ]

rank :: Thing -> Int
rank x = length([ y | y <- (propertiesOnlyOf x) ])

--rank A returns 0 whereas the others return 1; my hypothesis of A being the odd one out due to lack of uniqueness was correct.
--rank A returns 0 as it has no unique properties, and thus is the odd one out