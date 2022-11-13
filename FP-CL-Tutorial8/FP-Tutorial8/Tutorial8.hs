module Tutorial8 where  

import System.Random
import Test.QuickCheck

-- Importing the keymap module

--import KeymapList --comment out keymaplist to make it work
--import KeymapTreeSol
import KeymapTree
-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item

-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]

-- Exercise 1

getItems :: [Barcode] -> Catalogue -> [Item]
getItems bs c = [a | Just a <- [ get b c | b <- bs]]

-- Exercise 2

{-
*Tutorial8> db <- readDB
Done
(8.80 secs)
*Tutorial8> size db
104651
(0.11 secs)
*Tutorial8> ks <- samples 1000 db
(0.50 secs)
*Tutorial8> force (getItems ks db)
()
(0.03 secs)

If the database was two times bigger,
how would you expect the time to change?

the time would be two times bigger , as getItems is in linear time 
(O(n)) as the sample size stays the same. If samples also increased 
it would be exponential time.
-}

-- for Exercises 3--6 check KeymapTree.hs 

-- Exercise 7

{-
*Tutorial8> db <- readDB
Done
(4.22 secs)
*Tutorial8> size db
104651
(0.14 secs)
*Tutorial8> depth db
40
(0.11 secs)
*Tutorial8> ks <- loadKeys
(0.01 secs)
*Tutorial8> force (getItems ks db)
()
(0.04 secs)

If the database was two times bigger,
how would you expect the time to change?
The time would be slightly bigger or roughly the same as its in logarithmic time or O(logn)
-}

-- for Exercises 8--10 check KeymapTree.hs 

-- ** Input-output

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine (lines dbl))
            putStrLn (force (show db) `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

samples :: Int -> Catalogue -> IO [Barcode]
samples n db =
  do g <- newStdGen
     let allKeys = [ key | (key,item) <- toList db ]
     let indices = randomRs (0, length allKeys - 1) g
     let keys = take n [ allKeys !! i | i <- indices ]
     saveKeys keys
     return (force keys `seq` keys)

saveKeys :: [Barcode] -> IO ()
saveKeys = writeFile "keys.cache" . show

loadKeys :: IO [Barcode]
loadKeys = do
  keys <- read <$> readFile "keys.cache"
  return (force keys `seq` keys)

force :: [a] -> ()
force = foldr seq ()
