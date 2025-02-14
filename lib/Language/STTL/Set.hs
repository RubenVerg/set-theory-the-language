{-# LANGUAGE PatternSynonyms, TypeFamilies, ViewPatterns, OverloadedLists #-}

-- |
-- Module: Language.STTL.Set
--
-- The basic @Set@ type and operations.
module Language.STTL.Set
  ( Set
  , emptySet
  , setSingleton
  , makeSet'
  , makeSet
  , unSet'
  , unSet
  , pattern Set'
  , pattern Set
  , setCount
  , setUnion
  , setIntersection
  , setDifference
  , setSubset
  , setSuperset
  , setElement
  , setContains
  ) where

import qualified Language.STTL.Glyphs as G

import Data.List
import Numeric.Natural
import qualified Data.Set as Set
import Data.Hashable
import Data.Interned
import qualified GHC.IsList as IsL

data InternedSet' = IS' Id !(Set.Set Set)

instance Eq InternedSet' where
  (IS' a _) == (IS' b _) = a == b

instance Hashable InternedSet' where
  hashWithSalt s (IS' i _) = hashWithSalt s i

instance Interned InternedSet' where
  type Uninterned InternedSet' = Set.Set Set
  newtype Description InternedSet' = ISD (Set.Set Set) deriving (Eq, Hashable)
  describe = ISD
  identify = IS'
  cache = setCache

instance Uninternable InternedSet' where
  unintern (IS' _ s) = s

setCache :: Cache InternedSet'
setCache = mkCache
{-# NOINLINE setCache #-}

-- | A mathematical set, a collection of unique sets.
data Set = InternedSet !InternedSet'

instance Eq Set where
  (InternedSet a) == (InternedSet b) = a == b

instance Ord Set where
  compare (Set' a) (Set' b) = compare a b

instance Hashable Set where
  hashWithSalt s (InternedSet set) = hashWithSalt s set

instance IsL.IsList Set where
  type Item Set = Set
  fromList = makeSet
  toList = unSet

-- | The empty set \(\emptyset\).
emptySet :: Set
emptySet = InternedSet $ intern Set.empty

-- | A set containing just one element: \(\{x\}\).
setSingleton :: Set -> Set
setSingleton = InternedSet . intern . Set.singleton

-- | Turn a containers Set into a @Set@.
makeSet' :: Set.Set Set -> Set
makeSet' = InternedSet . intern

-- | Turn a list into a @Set@.
makeSet :: [Set] -> Set
makeSet = makeSet' . Set.fromList

-- | Contents of a set, as a containers Set
unSet' :: Set -> Set.Set Set
unSet' (InternedSet set) = unintern set

-- | Contents of a set.
unSet :: Set -> [Set]
unSet = Set.toAscList . unSet'

pattern Set' :: Set.Set Set -> Set
pattern Set' xs <- (unSet' -> xs) where
  Set' xs = makeSet' xs
{-# COMPLETE Set' #-}

pattern Set :: [Set] -> Set
pattern Set xs <- (unSet -> xs) where
  Set xs = makeSet xs
{-# COMPLETE Set #-}

-- | Default 'show' for sets shows the empty set with the symbol @∅@ and other sets with braces and commas.
instance Show Set where
  show (Set []) = [G.emptySet]
  show (Set xs) = [G.setOpen] ++ intercalate [G.elementSeparator, ' '] (map show xs) ++ [G.setClose]

-- | The count, cardinality or length of a set, i.e. the amount of elements: \(\#s\).
setCount :: Set -> Natural
setCount (Set' xs) = fromIntegral $ Set.size xs

-- | The union of two sets: a set containing all elements of both sets: \(a \cup b\).
setUnion :: Set -> Set -> Set
setUnion (Set' a) (Set' b) = makeSet' $ Set.union a b
  -- Data.List.union does not guarantee sorting, of course, so we can't use the ListSet constructor

-- | The intersection of two sets: a set containing all elements that are in both sets: \(a \cap b\).
setIntersection :: Set -> Set -> Set
setIntersection (Set' a) (Set' b) = makeSet' $ Set.intersection a b

-- | The difference of two sets: a set containing all elements that are in the first set but not in the second set: \(a \setminus b\).
setDifference :: Set -> Set -> Set
setDifference (Set' a) (Set' b) = makeSet' $ Set.difference a b

-- | Is a set a subset of another: are all its items part of another set? \(a \subseteq b\)
setSubset :: Set -> Set -> Bool
setSubset (Set' a) (Set' b) = a `Set.isSubsetOf` b

-- | Flipped version of 'setSubset: \(a \suberseteq b\)'.
setSuperset :: Set -> Set -> Bool
setSuperset = flip setSubset

-- | Is a set contained as an epement of another? \(a \in b\)
setElement :: Set -> Set -> Bool
setElement a (Set' b) = a `Set.member` b

-- | Flipped version of 'setElement': \(a \ni b\)
setContains :: Set -> Set -> Bool
setContains = flip setElement
