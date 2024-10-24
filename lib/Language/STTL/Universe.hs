-- |
-- Module: Language.STTL.Univere
--
-- Defines @Universe@s, contexts in which operations are performed.
module Language.STTL.Universe
  ( Universe(..)
  ) where

import Language.STTL.Set

-- | Collection of operations in a certain context
data Universe = Universe
  { universeChar :: Char -- ^ The double-struck character that represents this universe
  , universePlus :: Maybe (Set -> Set -> Either String Set) -- ^ Addition in this universe
  , universeTimes :: Maybe (Set -> Set -> Either String Set) -- ^ Multiplication in this universe
  }
