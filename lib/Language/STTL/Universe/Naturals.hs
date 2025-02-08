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
import Language.STTL.Multiverse
import Language.STTL.Context

import Text.Read

-- | The character for the naturals universe, @ℕ@.
naturalsChar :: Char
naturalsChar = 'ℕ'

-- | Turn a natural into a set.
naturalsParseNumeric :: Integer -> Context Set
naturalsParseNumeric n
  | n >= 0 = pure $ makeNatural $ fromInteger n
  | otherwise = throwError "Cannot parse negative number to natural"

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
-- Supports parsing numbers, showing, reading, @+@ and @×@.
naturals :: Universe
naturals = Universe
  { universeChar = naturalsChar
  , universeParseNumeric = Just naturalsParseNumeric
  , universeShow = Just naturalsShow
  , universeRead = Just naturalsRead
  , universePlus = Just naturalsPlus
  , universeMinus = Nothing
  , universeTimes = Just naturalsTimes
  , universeNegation = Nothing
  , universeUnion = Nothing
  , universeIntersection = Nothing
  }
