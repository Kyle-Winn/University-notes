module Tutorial7 where

import LSystem
import Test.QuickCheck

pathExample = Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#: Go 30

-- 1a. copy
copy :: Int -> Command -> Command
copy 0 cmd = Sit
copy x cmd = cmd :#: copy (x-1) cmd 

-- 1b. polygon
polygon :: Distance -> Int -> Command
polygon dist turns = copy turns (Go dist :#: Turn (360/(fromIntegral turns)))

-- 2. snowflake
snowflake :: Int -> Command
snowflake x = f x :#: n :#: n :#: f x :#: n :#: n :#: f x :#: n :#: n 
    where
    f 0 = Go 10
    f x = f (x-1) :#: p :#: f (x-1) :#: n :#: n :#: f (x-1) :#: p :#: f (x-1) 
    n   = Turn (-60)
    p   = Turn (60)

-- 3. sierpinski
--Using different colours for f and g as the example did.
sierpinski :: Int -> Command
sierpinski x =  f x
    where 
    f 0 = GrabPen red :#: Go 10
    f x = g (x-1) :#: p :#: f (x-1) :#: p :#: g (x-1)
    g 0 = GrabPen blue :#: Go 10
    g x = f (x-1) :#: n :#: g (x-1) :#: n :#: f (x-1)
    n   = Turn (-60)
    p   = Turn (60)

-- 4. hilbert
hilbert :: Int -> Command
hilbert =  undefined

-- 5. dragon
dragon :: Int -> Command
dragon =  undefined

-- ** Optional Material

-- 6a. split
split :: Command -> [Command]
split (p :#: q) = (split p) ++ (split q)
split (Sit) = []
split (p) = [p]

-- 6b. join
join :: [Command] -> Command
join []         = Sit
join (cmd:cmds) = cmd :#: join cmds

--Alternatively

joinFoldr :: [Command] -> Command
joinFoldr cmds = foldr (:#:) Sit cmds

prop_bothJoins :: [Command] -> Bool
prop_bothJoins cmds = joinFoldr cmds == join cmds
-- +++ OK, passed 100 tests.


-- 6c. equivalent
equivalent :: Command -> Command -> Bool
equivalent x y = split x == split y

-- 6d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join c = equivalent c (join (split c))

-- +++ OK, passed 100 tests.

prop_split :: Command -> Bool
prop_split cmd = and[ (x /= Sit) && (length(split(x)) == 1)| x <- split cmd]


-- 7. optimise

--q contains no Sit, Go 0 or Turn 0 commands, unless the command is equivalent to Sit.
--q contains no adjacent Go commands.
--q contains no adjacent Turn commands.

sitlessJoin :: [Command] -> Command
sitlessJoin cmd = foldr1 (:#:) cmd

optimise :: Command -> Command
optimise cmd = sitlessJoin  (handleGoandTurn (filter (/= Turn 0) (handleGoandTurn ( filter (/= Go 0) (split cmd)))))
    where 
        handleGoandTurn [] = []
        handleGoandTurn (Go x : Go y : xs)     = handleGoandTurn (Go (x+y) : xs)
        handleGoandTurn (Turn x : Turn y : xs) = handleGoandTurn (Turn (x+y) : xs)
        handleGoandTurn (x:xs)                 = x : handleGoandTurn xs
