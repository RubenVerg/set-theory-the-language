-- |
-- Module: Language.STTL.Universe.Naturals
--
-- The natural numbers universe.
module Language.STTL.Universe.Naturals
  ( naturalsChar
  , naturalsPlus
  , naturalsTimes
  , naturals
  ) where

import Language.STTL.Constructs
import Language.STTL.Set
import Language.STTL.Universe

-- | The character for the naturals universe, @ℕ@.
naturalsChar :: Char
naturalsChar = 'ℕ'

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
  , universePlus = Just naturalsPlus
  , universeTimes = Just naturalsTimes }
