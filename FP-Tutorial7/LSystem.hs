{-# LANGUAGE TypeSynonymInstances #-}
-- LSystem drawing module

-- {-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}

module LSystem (
  display,
  Command (..),
  Pen (..), black, white, red, green, blue,
  Distance, Angle,
  triangle, tree,
  examplePath, exampleTree, exampleSpiral,
  equivalent' -- for automarker
) where

import Test.QuickCheck
    ( elements, oneof, sized, Arbitrary(arbitrary) )
import Control.Monad (liftM2)

{- ** Uncomment for Generic Pretty Challenge
import Text.PrettyPrint.GenericPretty

deriving instance Generic Pen
deriving instance Out Pen
deriving instance Generic Command
deriving instance Out Command
-}

infixr 5 :#:

-- Points

data Pnt = Pnt Float Float
  deriving (Eq,Ord,Show)

instance Num Pnt where
  Pnt x y + Pnt x' y'  =  Pnt (x+x') (y+y')
  Pnt x y - Pnt x' y'  =  Pnt (x-x') (y-y')
  Pnt x y * Pnt x' y'  =  Pnt (x*x') (y*y')
  fromInteger          =  scalar . fromInteger
  abs (Pnt x y)        =  Pnt (abs x) (abs y)
  signum (Pnt x y)     =  Pnt (signum x) (signum y)

instance Fractional Pnt where
  Pnt x y / Pnt x' y'  =  Pnt (x/x') (y/y')
  fromRational         =  scalar . fromRational

scalar :: Float -> Pnt
scalar x  =  Pnt x x

scalarMax :: Pnt -> Pnt
scalarMax (Pnt x y)  =  scalar (x `max` y)

lub :: Pnt -> Pnt -> Pnt
Pnt x y `lub` Pnt x' y'  =  Pnt (x `max` x') (y `max` y')

glb :: Pnt -> Pnt -> Pnt
Pnt x y `glb` Pnt x' y'  =  Pnt (x `min` x') (y `min` y')


-- Colors

data Pen = Colour Float Float Float 
         | Inkless
           deriving (Eq, Ord, Show)

white, black, red, green, blue :: Pen
white = Colour 1.0 1.0 1.0
black = Colour 0.0 0.0 0.0
red   = Colour 1.0 0.0 0.0
green = Colour 0.0 1.0 0.0
blue  = Colour 0.0 0.0 1.0

-- Lines

data Ln = Ln Pen Pnt Pnt
  deriving (Eq,Ord,Show)


-- Commands for moving the turtle around
--  Turtles turn counter-clockwise and start facing up

type Angle    = Float
type Distance = Float
type Turtle   = (Pen,Angle,Pnt)

data Command = Go Distance
             | Turn Angle 
             | Sit
             | Command :#: Command
             | Branch Command
             | GrabPen Pen
               deriving (Eq, Ord, Show)

-- version that does not compare colour - used for autotest in the optional questions
equivalent' :: Command -> Command -> Bool
equivalent'  x y = (filterPen . split) x ==  (filterPen . split) y 
  where 
  isPen (GrabPen _) = True
  isPen _ = False
  filterPen = filter isPen 
  split :: Command -> [Command]
  split Sit             = []
  split (cmd1 :#: cmd2) =  split cmd1 ++ split cmd2
  split cmd             = [cmd]

execute :: Command -> [Ln]
execute c  =  lines
  where
  (lines, _)  =  f c (black, 0, Pnt 0 0)

  f :: Command -> Turtle -> ([Ln], Turtle)
  f (c :#: d) turtle             =  (clines ++ dlines, dturtle)
                                    where
                                    (clines, cturtle) = f c turtle
                                    (dlines, dturtle) = f d cturtle
  f (Branch c) turtle            =  (clines, turtle)
                                    where
                                    (clines, _) = f c turtle
  f (Go dst) (pen,ang,pnt)       =  ( [Ln pen pnt endpnt | pen /= Inkless], (pen,ang,endpnt))
                                    where
                                    endpnt = pnt + scalar dst * polar ang
  f (Turn delta) (pen,ang,pnt)   =  ([], (pen,ang-delta,pnt))
  f (GrabPen new) (_,ang,pnt)  =  ([], (new,ang,pnt))
  f Sit turtle                 =  ([], turtle)


-- Rescales all points in a list of lines
--  from an arbitrary scale
--  to (-1.-1) - (1.1)

rescale :: [Ln] -> [Ln]
rescale lines | null points  = []
              | otherwise    = map f lines
  where
    f (Ln pen p q)  =  Ln pen (g p) (g q)
    g p             =  swap ((p - p0) / s)
    points          =  [ r | Ln _ p q <- lines, r <- [p, q] ]
    hi              =  foldr1 lub points
    lo              =  foldr1 glb points
    s               =  scalarMax (hi - lo) * scalar 0.55
    p0              =  (hi + lo) * scalar 0.5
    swap (Pnt x y)  =  Pnt y x

polar :: Angle -> Pnt
polar ang  =  Pnt (cos radians) (sin radians)
  where
  radians  =  ang * 2 * pi / 360


-- Sample LSystems

triangle :: Int -> Command
triangle x  =  p :#: f x
  where
  f 0      = Go 10
  f x  = f (x-1) :#: p :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1)
  n        = Turn 90
  p        = Turn (-90)

tree :: Int -> Command
tree x  =  f x
  where
  f 0      = GrabPen red :#: Go 10
  f x  = g (x-1) :#: Branch (n :#: f (x-1))
                 :#: Branch (p :#: f (x-1))
                 :#: Branch (g (x-1) :#: f (x-1))
  g 0      = GrabPen blue :#: Go 10
  g x  = g (x-1) :#: g (x-1)
  n        = Turn 45
  p        = Turn (-45)



-- Generators for QuickCheck

instance Arbitrary Pen where
    arbitrary  =  sized pen
        where
          pen _  =  elements [black,red,green,blue,white,Inkless]


instance Arbitrary Command where
    arbitrary  =  sized cmd 
        where
          cmd n  |  n <= 0     =  oneof [return Sit,
                                         Go . abs <$> arbitrary,
                                         Turn <$> arbitrary ]
                 |  otherwise  =  liftM2 (:#:) (cmd (n `div` 2)) (cmd (n `div`2))


parseLine :: Ln -> String
parseLine (Ln (Colour r g b) (Pnt x1 y1) (Pnt x2 y2))
  = 
    "\n<line x1=\"" ++ x1' ++ "%\"" ++
          "y1=\"" ++ y1' ++ "%\"" ++
          "x2=\"" ++ x2' ++ "%\"" ++
          "y2=\"" ++ y2' ++ "%\"" ++
          "style=\"stroke:rgb" ++ rgb ++ "\" />"
  where
    x1' = show $ scaleToSvg x1
    y1' = show $ scaleToSvg (-y1)
    x2' = show $ scaleToSvg x2
    y2' = show $ scaleToSvg (-y2) 
    rgb = "(" ++ show (r*255) ++ "," ++ show (g*255) ++"," ++ show (b*255) ++ ")"

scaleToSvg x =  (x + 1) / 2 * 100.0 
    
toHtmlBody :: Command -> String
toHtmlBody = concatMap parseLine . rescale . execute

wrapSvgTag:: String -> String
wrapSvgTag x = "<svg height=\"" ++ "750" ++ "\" width=\"" ++ "750" ++ "\">" ++ x ++ "</svg>"

display :: Command -> IO ()
display = writeFile "output.html" . wrapSvgTag . toHtmlBody

-- Some examples

examplePath = Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#: Go 30 :#: Go 50
exampleTree = tree 4
exampleSpiral = Go 30.0 :#: Turn 30.0 :#: Go 35.0 :#: Turn 30.0 :#: Go 40.0 :#: Turn 30.0 :#: Go 45.0 :#: Turn 30.0

