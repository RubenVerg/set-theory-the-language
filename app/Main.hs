module Main where

import Language.STTL.Constructs
import Language.STTL.Set

main :: IO ()
main = do
  print emptySet
  print $ makeSet [emptySet, emptySet]
  print $ makeSet [emptySet, makeSet [emptySet]]
  print $ makeNatural 5
  print $ makePair (makeNatural 2, makeNatural 3)
