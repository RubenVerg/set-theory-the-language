-- |
-- Module: Language.STTL.Interpreter
--
-- Interpreter for Set Theory: The Language.
module Language.STTL.Interpreter
  ( interpret
  , run
  ) where

import qualified Language.STTL.Glyphs as G
import Language.STTL.Parser
import Language.STTL.Set
import Language.STTL.Constructs
import Language.STTL.Universe
import Language.STTL.DefaultUniverses

import Data.Composition

withUniverse :: Char -> (Universe -> Either String a) -> Either String a
withUniverse u f = case lookup u universes of
  Nothing -> Left $ "Universe " ++ [u] ++ " does not exist"
  Just un -> f un

-- | Interpret an 'AST'.
interpret :: AST -> Either String Set
interpret LeafEmptySet = pure emptySet
interpret (LeafNumeric n u) = withUniverse u $ \un -> case universeParseNumeric un of
  Nothing -> Left $ "Universe " ++ [u] ++ " does not support numeric literals"
  Just f -> f n
interpret (BranchSetLiteral xs) = makeSet <$> mapM interpret xs
interpret (BranchMonad c x)
  | c == G.count = makeNatural . setCount <$> interpret x
  | otherwise = Left $ "Unknown operator " ++ [c]
interpret (BranchDyad c x y)
  | c == G.union = setUnion <$> interpret x <*> interpret y
  | c == G.intersection = setIntersection <$> interpret x <*> interpret y
  | c == G.difference = setDifference <$> interpret x <*> interpret y
  | c == G.cartesianProduct = cartesianProduct <$> interpret x <*> interpret y
  | c == G.subset = makeBoolean .: setSubset <$> interpret x <*> interpret y
  | c == G.superset = makeBoolean .: setSuperset <$> interpret x <*> interpret y
  | c == G.element = makeBoolean .: setElement <$> interpret x <*> interpret y
  | c == G.contains = makeBoolean .: setContains <$> interpret x <*> interpret y
  | c == G.pair = curry makePair <$> interpret x <*> interpret y
  | otherwise = Left $ "Unknown operator " ++ [c]
interpret (BranchUniversalMonad c u _) = Left $ "Unknown operator " ++ [c] ++ " in universe " ++ [u]
interpret (BranchUniversalDyad c u x y) = withUniverse u $ \un -> let
  evalOrNot Nothing = Left $ "Universe " ++ [u] ++ " does not support operation " ++ [c]
  evalOrNot (Just f) = do
    x' <- interpret x
    y' <- interpret y
    f x' y'
  in case c of
    _ | c == G.plus -> evalOrNot $ universePlus un
    _ | c == G.cartesianProduct -> evalOrNot $ universeTimes un
    _ -> Left $ "Unknown operator " ++ [c] ++ " in universe " ++ [u]

-- | Parse and interpret in a single function.
run :: FilePath -> String -> Either String Set
run file source = do
  ast <- parse file source
  interpret ast
