module KeymapTree ( Keymap(..),
                    invariant, keys,
                    size, depth, get, set, select,
                    toList, fromList,
                    filterLT, filterGT, merge,
                  )
where

-- Modules for testing
  
import Test.QuickCheck
import Control.Monad
import Data.List
  
-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a)
                deriving (Eq, Show)

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf 
                               (Node 4 40 Leaf Leaf ))

-- Invariant

invariant :: Ord k => Keymap k a -> Bool
invariant Leaf  =  True
invariant (Node k _ left right)  =  all (< k) (keys left) &&
                                    all (> k) (keys right) &&
                                    invariant left &&
                                    invariant right

keys :: Keymap k a -> [k]
keys Leaf  =  []
keys (Node k _ left right)  =  keys left ++ [k] ++ keys right

size :: Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

depth :: Keymap k a -> Int
depth Leaf = 0
depth (Node _ _ left right) = 1 + (depth left `max` depth right)

-- Exercise 3


toList :: Keymap k a -> [(k,a)]
toList Leaf  =  []
toList (Node x y left right)  =  toList left ++ [(x,y)] ++ toList right


-- Exercise 4

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = go
    where go Leaf = Node key value Leaf Leaf
          go (Node k v left right) | key == k = Node k v left right
                                   | key < k  = Node k v (go left) right
                                   | key > k  = Node k v left (go right)
                                     

-- Exercise 5

get :: Ord k => k -> Keymap k a -> Maybe a
get k Leaf = Nothing
get key (Node k v left right)
    | key == k  = Just v
    | key < k   = get key left
    | key > k   = get key right

prop_set_get :: Int -> String -> Keymap Int String -> Bool
prop_set_get k v db = get k (set k v db) == Just v

-- Exercise 6

fromList :: Ord k => [(k,a)] -> Keymap k a
fromList [] = Leaf
fromList ((k,v):xs) = set k v (fromList xs) 

prop_toList_fromList :: [Int] -> [String] -> Bool
prop_toList_fromList xs ys  =  toList (fromList zs) == sort zs
  where zs = zip (nub xs) ys

-- +++ OK, passed 100 tests.

-- ** Optional Material

-- Exercise 8

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT key keymap = fromList(filter (\x -> (fst x) < key) (toList keymap))

                               
filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT key keymap = fromList(filter (\x -> (fst x) > key) (toList keymap))

-- Exercise 9
                                     
merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge keymap1 keymap2 =  fromList (toList keymap1 ++ toList keymap2)

prop_merge ::  (Ord k, Eq a) => Keymap k a -> Keymap k a -> Bool
prop_merge km1 km2 = toList (merge km1 km2)  == (nub (toList km1 ++ toList km2))

-- Exercise 10

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select pred Leaf  = Leaf 
select pred (Node k v left right)
      | pred v    = Node k v (select pred left) (select pred right)
      | otherwise = merge (select pred left) (select pred right)


-- Instances for QuickCheck -----------------------------

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
    arbitrary = liftM fromList (liftM2 zip (liftM nub arbitrary) arbitrary)
