-- Indexed data represented as a list

module KeymapList ( Keymap,
                    invariant, keys,
                    size, get, set, del, select,
                    toList, fromList
                  )
where

import Test.QuickCheck 
import Data.List (nub,sort)
import Data.List.Extra (anySame)
import Control.Monad (liftM,liftM2)

data Keymap k a = K [(k,a)]
                deriving (Eq,Show)

invariant :: Eq k => Keymap k a -> Bool
invariant db  = not (anySame (keys db))

keys :: Keymap k a -> [k]
keys (K xs)   =  map fst xs

size :: Keymap k a -> Int
size (K xs) = length xs

get :: Eq k => k -> Keymap k a -> Maybe a
get key (K xs) = lookup key xs

set :: Eq k => k -> a -> Keymap k a -> Keymap k a
set key value (K xs) = K (ins  xs)
    where
      ins [] = [(key,value)]
      ins ((k,v):xs) | k == key  = (k,value) : xs
                     | otherwise = (k,v) : ins xs

del :: Eq k => k -> Keymap k a -> Keymap k a
del key (K xs) = K (filter ((/= key) . fst) xs)

select :: Eq k => (a -> Bool) -> Keymap k a -> Keymap k a
select f (K xs) = K (filter (f . snd) xs)

toList :: Keymap k a -> [(k,a)]
toList (K xs) = xs

fromList :: [(k,a)] -> Keymap k a
fromList xs = K xs

-- for QuickCheck
instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
  arbitrary = liftM K (liftM2 zip (liftM nub (listOf arbitrary)) arbitrary)
