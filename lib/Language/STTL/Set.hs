-- |
-- Module: Language.STTL.Set
--
-- The basic @Set@ type and operations.
module Language.STTL.Set
  ( Set
  , emptySet
  , setSingleton
  , makeSet
  , unSet
  , setCount
  , setUnion
  , setIntersection
  , setDifference
  ) where

import qualified Language.STTL.Glyphs as G

import Data.List
import Numeric.Natural

-- | A mathematical set, a collection of unique sets.
newtype Set = ListSet [Set]
  deriving (Eq, Ord)

-- | The empty set \(\emptyset\).
emptySet :: Set
emptySet = ListSet []

-- | A set containing just one element: \(\{x\}\).
setSingleton :: Set -> Set
setSingleton x = ListSet [x]

-- | Turn a list into a @Set@.
makeSet :: [Set] -> Set
makeSet = ListSet . sort . nub

-- | Contents of a set.
unSet :: Set -> [Set]
unSet (ListSet xs) = xs

-- | Default 'show' for sets shows the empty set with the symbol @∅@ and other sets with braces and commas.
instance Show Set where
  show (ListSet []) = [G.emptySet]
  show (ListSet xs) = [G.setOpen] ++ intercalate [G.elementSeparator, ' '] (map show xs) ++ [G.setClose]

-- | The count, cardinality or length of a set, i.e. the amount of elements: \(\#s\).
setCount :: Set -> Natural
setCount (ListSet xs) = genericLength xs

-- | The union of two sets: a set containing all elements of both sets: \(a \cup b\).
setUnion :: Set -> Set -> Set
setUnion (ListSet a) (ListSet b) = makeSet $ a ++ b
  -- Data.List.union does not guarantee sorting, of course, so we can't use the ListSet constructor

-- | The intersection of two sets: a set containing all elements that are in both sets: \(a \cap b\).
setIntersection :: Set -> Set -> Set
setIntersection (ListSet a) (ListSet b) = ListSet $ intersect a b

-- | The difference of two sets: a set containing all elements that are in the first set but not in the second set: \(a \setminus b\).
setDifference :: Set -> Set -> Set
setDifference (ListSet a) (ListSet b) = ListSet $ a \\ b
