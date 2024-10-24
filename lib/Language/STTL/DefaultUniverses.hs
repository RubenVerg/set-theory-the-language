-- |
-- Module: Language.STTL.DefaultUniverses
--
-- Contains a collection of all default universes.
module Language.STTL.DefaultUniverses (universes) where

import Language.STTL.Universe
import Language.STTL.Universe.Naturals
import Language.STTL.Universe.Integers

-- | Map of universes.
universes :: [(Char, Universe)]
universes = (\x -> (universeChar x, x)) <$>
  [ naturals
  , integers
  ]