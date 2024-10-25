module Language.STTL.Biverse.NaturalsIntegers
  ( naturalToInteger
  , naturalsIntegers
  , integerToNatural
  , integersNaturals
  ) where

import Language.STTL.Constructs
import Language.STTL.Set
import Language.STTL.Context
import Language.STTL.Multiverse
import Language.STTL.Universe.Naturals
import Language.STTL.Universe.Integers

naturalToInteger :: Set -> Context Set
naturalToInteger = pure . makeInteger . toInteger . unNatural

integerToNatural :: Set -> Context Set
integerToNatural s = case unInteger s of
  Nothing -> throwError "Not an integer!"
  Just i -> naturalsParseNumeric i

injectNaturalToInteger :: Set -> Context Set
injectNaturalToInteger s = let
  n = unNatural s
  in case even n of
    True -> pure $ makeInteger $ toInteger $ n `div` 2
    False -> pure $ makeInteger $ negate $ toInteger $ (n + 1) `div` 2

injectIntegerToNatural :: Set -> Context Set
injectIntegerToNatural s = case unInteger s of
  Nothing -> throwError "Not an integer!"
  Just i | i < 0 -> pure $ makeNatural $ fromInteger $ subtract 1 $ 2 * negate i
  Just i -> pure $ makeNatural $ fromInteger $ 2 * i

naturalsIntegers :: Biverse
naturalsIntegers = Biverse
  { biverseChars = (naturalsChar, integersChar)
  , biverseConvert = Just naturalToInteger
  , biverseInject = Just injectNaturalToInteger
  }

integersNaturals :: Biverse
integersNaturals = Biverse
  { biverseChars = (integersChar, naturalsChar)
  , biverseConvert = Just integerToNatural
  , biverseInject = Just injectIntegerToNatural
  }

