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
snowflake x = undefined

-- 3. sierpinski
sierpinski :: Int -> Command
sierpinski =  undefined
     
-- 4. hilbert
hilbert :: Int -> Command
hilbert =  undefined

-- 5. dragon
dragon :: Int -> Command
dragon =  undefined

-- ** Optional Material

-- 6a. split
split :: Command -> [Command]
split =  undefined

-- 6b. join
join :: [Command] -> Command
join =  undefined

-- 6c. equivalent
equivalent :: Command -> Command -> Bool
equivalent =  undefined

-- 6d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join =  undefined

prop_split :: Command -> Bool
prop_split =  undefined

-- 7. optimise
optimise :: Command -> Command
optimise =  undefined
