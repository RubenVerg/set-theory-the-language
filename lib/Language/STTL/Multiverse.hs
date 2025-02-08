-- |
-- Module: Language.STTL.Multiverse
--
-- Defines @Universe@s, contexts in which operations are performed, and @Biverse@s, contexts in which operations that relate to two universes are performed.
module Language.STTL.Multiverse
  ( Universe(..)
  , Biverse(..)
  ) where

import Language.STTL.Set
import Language.STTL.Context

-- | Collection of operations in a certain context
data Universe = Universe
  { universeChar :: Char -- ^ The double-struck character that represents this universe.
  , universeShow :: Maybe (Set -> Context String)
  , universeRead :: Maybe (String -> Context Set)
  , universeParseNumeric :: Maybe (Integer -> Context Set) -- ^ Parsing a number to this universe.
  , universePlus :: Maybe (Set -> Set -> Context Set) -- ^ Addition in this universe.
  , universeMinus :: Maybe (Set -> Set -> Context Set) -- ^ Subtraction in this universe.
  , universeTimes :: Maybe (Set -> Set -> Context Set) -- ^ Multiplication in this universe.
  , universeNegation :: Maybe (Set -> Context Set) -- ^ Negation in this universe.
  }

-- | Collection of operations in two contexts.
data Biverse = Biverse
  { biverseChars :: (Char, Char)
  , biverseConvert :: Maybe (Set -> Context Set)
  , biverseInject :: Maybe (Set -> Context Set)
  }
