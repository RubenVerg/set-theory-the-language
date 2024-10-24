-- |
-- Module: Language.STTL.Universe.Naturals
--
-- The natural numbers universe.
module Language.STTL.Universe.Naturals
  ( naturalsChar
  , naturalsParseNumeric
  , naturalsShow
  , naturalsPlus
  , naturalsTimes
  , naturals
  ) where

import Language.STTL.Constructs
import Language.STTL.Set
import Language.STTL.Universe
import Language.STTL.Context

import Numeric.Natural

-- | The character for the naturals universe, @ℕ@.
naturalsChar :: Char
naturalsChar = 'ℕ'

-- | Turn a natural into a set.
naturalsParseNumeric :: Natural -> Context Set
naturalsParseNumeric = pure . makeNatural

-- | Representation of a natural.
naturalsShow :: Set -> Context String
naturalsShow = pure . show . setCount

-- | Addition in the naturals universe
naturalsPlus :: Set -> Set -> Context Set
naturalsPlus a b = pure $ makeNatural $ unNatural a + unNatural b

-- | Multiplication in the naturals universe.
naturalsTimes :: Set -> Set -> Context Set
naturalsTimes a b = pure $ makeNatural $ unNatural a * unNatural b

-- | The natural numbers universe.
--
-- Supports @+@ and @×@.
naturals :: Universe
naturals = Universe
  { universeChar = naturalsChar
  , universeParseNumeric = Just naturalsParseNumeric
  , universeShow = Just naturalsShow
  , universePlus = Just naturalsPlus
  , universeTimes = Just naturalsTimes }
