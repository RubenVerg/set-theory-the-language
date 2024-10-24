-- |
-- Module: Language.STTL.Interpreter
--
-- Interpreter for Set Theory: The Language.
module Language.STTL.Interpreter
  ( interpretExpr
  , interpret
  , run
  ) where

import qualified Language.STTL.Glyphs as G
import Language.STTL.Parser
import Language.STTL.Set
import Language.STTL.Constructs
import Language.STTL.Universe
import Language.STTL.DefaultUniverses
import Language.STTL.Context

import Data.Composition
import Data.Functor
import System.IO

withUniverse :: Char -> (Universe -> Context a) -> Context a
withUniverse u f = case lookup u universes of
  Nothing -> throwError $ "Universe " ++ [u] ++ " does not exist"
  Just un -> f un

-- | Interpret an 'Expr'.
interpretExpr :: Expr -> Context Set
interpretExpr ExprEmptySet = pure emptySet
interpretExpr (ExprNumeric n u) = withUniverse u $ \un -> case universeParseNumeric un of
  Nothing -> throwError $ "Universe " ++ [u] ++ " does not support numeric literals"
  Just f -> f n
interpretExpr (ExprSetLiteral xs) = makeSet <$> mapM interpretExpr xs
interpretExpr ExprGet = do
  liftIO $ putStr "input> "
  liftIO $ hFlush stdout
  l <- liftIO getLine
  parseExpr "<input>" l >>= interpretExpr
interpretExpr (ExprUniversalGet u) = withUniverse u $ \un -> case universeRead un of
  Nothing -> throwError $ "Universe " ++ [u] ++ " does not support reading input"
  Just f -> do
    liftIO $ putStr $ "input " ++ [u] ++ "> "
    liftIO $ hFlush stdout
    l <- liftIO getLine
    f l
interpretExpr (ExprMonad c x)
  | c == G.count = makeNatural . setCount <$> interpretExpr x
  | otherwise = throwError $ "Unknown operator " ++ [c]
interpretExpr (ExprDyad c x y)
  | c == G.union = setUnion <$> interpretExpr x <*> interpretExpr y
  | c == G.intersection = setIntersection <$> interpretExpr x <*> interpretExpr y
  | c == G.difference = setDifference <$> interpretExpr x <*> interpretExpr y
  | c == G.cartesianProduct = cartesianProduct <$> interpretExpr x <*> interpretExpr y
  | c == G.subset = makeBoolean .: setSubset <$> interpretExpr x <*> interpretExpr y
  | c == G.superset = makeBoolean .: setSuperset <$> interpretExpr x <*> interpretExpr y
  | c == G.element = makeBoolean .: setElement <$> interpretExpr x <*> interpretExpr y
  | c == G.contains = makeBoolean .: setContains <$> interpretExpr x <*> interpretExpr y
  | c == G.pair = curry makePair <$> interpretExpr x <*> interpretExpr y
  | otherwise = throwError $ "Unknown operator " ++ [c]
interpretExpr (ExprUniversalMonad c u _) = throwError $ "Unknown operator " ++ [c] ++ " in universe " ++ [u]
interpretExpr (ExprUniversalDyad c u x y) = withUniverse u $ \un -> let
  evalOrNot Nothing = throwError $ "Universe " ++ [u] ++ " does not support operation " ++ [c]
  evalOrNot (Just f) = do
    x' <- interpretExpr x
    y' <- interpretExpr y
    f x' y'
  in case c of
    _ | c == G.plus -> evalOrNot $ universePlus un
    _ | c == G.minus -> evalOrNot $ universeMinus un
    _ | c == G.cartesianProduct -> evalOrNot $ universeTimes un
    _ -> throwError $ "Unknown operator " ++ [c] ++ " in universe " ++ [u]

-- | Interpret a 'Stmt'.
interpret :: Stmt -> Context (Maybe Set)
interpret (StmtExpr e) = Just <$> interpretExpr e
interpret (StmtPrint e) = (interpretExpr e >>= liftIO . print) $> Nothing
interpret (StmtUniversalPrint u e) = withUniverse u $ \un -> do
  case universeShow un of 
    Nothing -> throwError $ "Universe " ++ [u] ++ " does not support printing"
    Just sh -> do
      x <- interpretExpr e >>= sh
      liftIO $ putStrLn x
      pure Nothing

-- | Parse and interpret in a single function.
run :: FilePath -> String -> Context (Maybe Set)
run file source = do
  ast <- parse file source
  interpret ast
