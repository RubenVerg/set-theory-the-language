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

-- | Interpret an 'AST'.
interpret :: AST -> Either String Set
interpret LeafEmptySet = pure emptySet
interpret (BranchSetLiteral xs) = makeSet <$> mapM interpret xs
interpret (BranchMonad c x) | c == G.count = makeNatural . setCount <$> interpret x
                            | otherwise = Left $ "Unknown operator " ++ [c]
interpret (BranchDyad c x y) | c == G.union = setUnion <$> interpret x <*> interpret y
                             | c == G.intersection = setIntersection <$> interpret x <*> interpret y
                             | c == G.difference = setDifference <$> interpret x <*> interpret y
                             | otherwise = Left $ "Unknown operator " ++ [c]

-- | Parse and interpret in a single function.
run :: FilePath -> String -> Either String Set
run file source = do
  ast <- parse file source
  interpret ast
