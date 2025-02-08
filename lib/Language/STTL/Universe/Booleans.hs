-- |
-- Module: Language.STTL.Universe.Booleans
--
-- The boolean universe.
module Language.STTL.Universe.Booleans
  ( booleansChar
  , booleansShow
  , booleansRead
  , booleansUnion
  , booleansIntersection
  , booleansNegation
  , booleans
  ) where

import Language.STTL.Context
import Language.STTL.Set
import Language.STTL.Constructs
import Language.STTL.Multiverse

import Data.Bool

booleansChar :: Char
booleansChar = '𝔹'

booleansShow :: Set -> Context String
booleansShow = pure . bool "⊥" "⊤" . unBoolean

booleansRead :: String -> Context Set
booleansRead s = case s of
  "⊥" -> pure booleanFalse
  "⊤" -> pure booleanTrue
  _ -> throwError $ "Could not parse boolean: " ++ s

booleansUnion :: Set -> Set -> Context Set
booleansUnion a b = pure $ makeBoolean $ unBoolean a || unBoolean b

booleansIntersection :: Set -> Set -> Context Set
booleansIntersection a b = pure $ makeBoolean $ unBoolean a && unBoolean b

booleansNegation :: Set -> Context Set
booleansNegation a = pure $ makeBoolean $ not $ unBoolean a

booleans :: Universe
booleans = Universe
  { universeChar = booleansChar
  , universeParseNumeric = Nothing
  , universeShow = Just booleansShow
  , universeRead = Just booleansRead
  , universePlus = Nothing
  , universeMinus = Nothing
  , universeTimes = Nothing
  , universeNegation = Just booleansNegation
  , universeUnion = Just booleansUnion
  , universeIntersection = Just booleansIntersection
  }
