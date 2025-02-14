{-# LANGUAGE OverloadedLists #-}
-- |
-- Module: Language.STTL.Constructs
--
-- Higher-level constructs built on top of sets.
module Language.STTL.Constructs
  ( -- * Pairs
    --
    -- |
    -- Pairs are represented as a set of (a set containing the first element) and (a set containing both elements).
    -- When the first and second elements are the same, this collapses to the set containing the set containing the value.
    --
    -- \[ (a, b) \leftrightarrow \{\{a\}, \{a, b\}\} \]
    makePair
  , unPair
  , isPair
  , pairFst
  , pairSnd
  , cartesianProduct
    -- * Naturals
    --
    -- | Natural numbers are represented recursively:
    --
    --   * zero is the empty set
    --   * the successor of a natural \(n\) is \(n \cup \{n\}\)
  , makeNatural
  , unNatural
  , naturalZero
  , naturalSucc
    -- * Integers
    --
    -- | Integers are represented as pairs of natural numbers, where the first element is the "positive" part and the second element is the "negative" part.
  , makeInteger
  , unInteger
    -- * Booleans
    --
    -- | @False@ is represented as \(\emptyset\) and @True@ as \(\{\emptyset\}\).
  , makeBoolean
  , unBoolean
  , booleanFalse
  , booleanTrue
  ) where

import Language.STTL.Set
import Language.STTL.Util

import Numeric.Natural
import qualified Data.Set as Set

-- | Construct a 'Set' pair from a Haskell pair.
makePair :: (Set, Set) -> Set
makePair (a, b) = [[a], [a, b]]

-- | Extract the contents of a pair.
unPair :: Set -> Maybe (Set, Set)
unPair xs = case xs of
  [s] -> do
    [v] <- pure s
    pure (v, v)
  [a, b] -> case (setCount a, setCount b) of
    (1, 2) -> do
      [v'] <- pure a
      [v, w] <- pure b
      if v == v' then pure (v, w) else Nothing
    (2, 1) -> do
      [v'] <- pure b
      [w, v] <- pure a
      if v == v' then pure (v, w) else Nothing
    _ -> Nothing
  _ -> Nothing

-- | Is the given set a pair?
isPair :: Set -> Bool
isPair = (Nothing /=) . unPair

-- | Extract the first element of a pair.
pairFst :: Set -> Maybe Set
pairFst = fmap fst . unPair

-- | Extract the second element of a pair.
pairSnd :: Set -> Maybe Set
pairSnd = fmap snd . unPair

-- | Cartesian product of two sets: \(a \times b\) where
--
-- \[ A \times B = \{ (a, b) | a \in A, b \in B \} \]
cartesianProduct :: Set -> Set -> Set
cartesianProduct (Set' x) (Set' y) = Set' $ Set.map makePair $ Set.cartesianProduct x y

-- | Construct a 'Set' natural number from a 'Natural'.
makeNatural :: Natural -> Set
makeNatural 0 = naturalZero
makeNatural n = naturalSucc $ makeNatural $ n - 1

-- | Extract the value of a natural number Set.
-- This is very lax, accepting any set, and just returns its length.
unNatural :: Set -> Natural
unNatural = setCount

-- | The natural representing \(0\).
naturalZero :: Set
naturalZero = emptySet

-- | The natural representing \(n + 1\).
naturalSucc :: Set -> Set
naturalSucc s@(Set s') = Set' $ Set.fromAscList $ s' :> s

-- | Construct a 'Set' integer from an 'Integer'.
makeInteger :: Integer -> Set
makeInteger n | n > 0 = makePair (makeNatural $ fromIntegral n, naturalZero)
              | n < 0 = makePair (naturalZero, makeNatural $ fromIntegral $ negate n)
              | otherwise = makePair (naturalZero, naturalZero)

-- | Extract the value of an integer Set.
unInteger :: Set -> Maybe Integer
unInteger s = do
  (p, n) <- unPair s
  pure $ toInteger (unNatural p) - toInteger (unNatural n)

-- | Turn a boolean into its set representation.
makeBoolean :: Bool -> Set
makeBoolean False = booleanFalse
makeBoolean True = booleanTrue

-- | Convert empty sets to @False@ and everything else to @True@.
unBoolean :: Set -> Bool
unBoolean s = if s == booleanFalse then False else True

-- | The boolean representing @False@.
booleanFalse :: Set
booleanFalse = []

-- | The boolean representing @True@.
booleanTrue :: Set
booleanTrue = [[]]
