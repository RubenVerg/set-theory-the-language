-- |
-- Module: Language.STTL.Universe.Naturals
--
-- The natural numbers universe.
module Language.STTL.Universe.Naturals
  ( naturalsChar
  , naturalsParseNumeric
  , naturalsShow
  , naturalsRead
  , naturalsPlus
  , naturalsTimes
  , naturals
  ) where

import Language.STTL.Constructs
import Language.STTL.Set
import Language.STTL.Universe
import Language.STTL.Context

import Numeric.Natural
import Text.Read

-- | The character for the naturals universe, @ℕ@.
naturalsChar :: Char
naturalsChar = 'ℕ'

-- | Turn a natural into a set.
naturalsParseNumeric :: Natural -> Context Set
naturalsParseNumeric = pure . makeNatural

-- | Representation of a natural.
naturalsShow :: Set -> Context String
naturalsShow = pure . show . setCount

-- | Parsing of a natural.
naturalsRead :: String -> Context Set
naturalsRead s = case readMaybe s of
  Nothing -> throwError $ "Could not parse natural: " ++ s
  Just n -> pure $ makeNatural n

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
  , universeRead = Just naturalsRead
  , universePlus = Just naturalsPlus
  , universeTimes = Just naturalsTimes }
