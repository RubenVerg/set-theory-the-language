{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
-- |
-- Module: Language.STTL.Util
--
-- Various utilities
module Language.STTL.Util
  ( snoc
  , Data.List.unsnoc
  , pattern (:>)
  ) where

import Data.List

snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

pattern (:>) :: [a] -> a -> [a]
pattern xs :> x <- (unsnoc -> Just (xs, x)) where
  (:>) = snoc
{-# COMPLETE [], (:>) #-}
