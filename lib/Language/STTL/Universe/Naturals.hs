-- |
-- Module: Language.STTL.Universe.Naturals
--
-- The natural numbers universe.
module Language.STTL.Universe.Naturals
  ( naturalsChar
  , naturalsParseNumeric
  , naturalsPlus
  , naturalsTimes
  , naturals
  ) where

import Language.STTL.Constructs
import Language.STTL.Set
import Language.STTL.Universe

import Numeric.Natural

-- | The character for the naturals universe, @ℕ@.
naturalsChar :: Char
naturalsChar = 'ℕ'

-- | Turn a natural into a set.
naturalsParseNumeric :: Natural -> Either String Set
naturalsParseNumeric = pure . makeNatural

-- | Addition in the naturals universe
naturalsPlus :: Set -> Set -> Either String Set
naturalsPlus a b = pure $ makeNatural $ unNatural a + unNatural b

-- | Multiplication in the naturals universe.
naturalsTimes :: Set -> Set -> Either String Set
naturalsTimes a b = pure $ makeNatural $ unNatural a * unNatural b

-- | The natural numbers universe.
--
-- Supports @+@ and @×@.
naturals :: Universe
naturals = Universe
  { universeChar = naturalsChar
  , universeParseNumeric = Just naturalsParseNumeric
  , universePlus = Just naturalsPlus
  , universeTimes = Just naturalsTimes }
