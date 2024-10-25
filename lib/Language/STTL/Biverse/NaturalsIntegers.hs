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

naturalsIntegers :: Biverse
naturalsIntegers = Biverse
  { biverseChars = (naturalsChar, integersChar)
  , biverseConvert = Just naturalToInteger
  }

integersNaturals :: Biverse
integersNaturals = Biverse
  { biverseChars = (integersChar, naturalsChar)
  , biverseConvert = Just integerToNatural
  }

