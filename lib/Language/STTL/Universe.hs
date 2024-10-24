-- |
-- Module: Language.STTL.Univere
--
-- Defines @Universe@s, contexts in which operations are performed.
module Language.STTL.Universe
  ( Universe(..)
  ) where

import Language.STTL.Set
import Language.STTL.Context

import Numeric.Natural

-- | Collection of operations in a certain context
data Universe = Universe
  { universeChar :: Char -- ^ The double-struck character that represents this universe.
  , universeShow :: Maybe (Set -> Context String)
  , universeParseNumeric :: Maybe (Natural -> Context Set) -- ^ Parsing a number to this universe.
  , universePlus :: Maybe (Set -> Set -> Context Set) -- ^ Addition in this universe.
  , universeTimes :: Maybe (Set -> Set -> Context Set) -- ^ Multiplication in this universe.
  }
