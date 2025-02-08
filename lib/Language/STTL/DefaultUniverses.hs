-- |
-- Module: Language.STTL.DefaultUniverses
--
-- Contains a collection of all default universes.
module Language.STTL.DefaultUniverses
  ( universes
  , biverses
  ) where

import Language.STTL.Multiverse
import Language.STTL.Universe.Booleans
import Language.STTL.Universe.Naturals
import Language.STTL.Universe.Integers
import Language.STTL.Biverse.NaturalsIntegers (naturalsIntegers, integersNaturals)

-- | Map of universes.
universes :: [(Char, Universe)]
universes = (\x -> (universeChar x, x)) <$>
  [ booleans
  , naturals
  , integers
  ]

-- | Map of biverses.
biverses :: [((Char, Char), Biverse)]
biverses = (\x -> (biverseChars x, x)) <$>
  [ naturalsIntegers
  , integersNaturals
  ]