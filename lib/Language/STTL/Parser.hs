-- | Module: Language.STTL.Parser
--
-- Parser for Set Theory: The Language.
module Language.STTL.Parser
  ( Expr(..)
  , Stmt(..)
  , Language.STTL.Parser.parse
  ) where

import qualified Language.STTL.Glyphs as G
import Language.STTL.Context

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Functor
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Bifunctor
import Control.Monad.Combinators.Expr
import Numeric.Natural
import Data.Composition

-- | Expression abstract syntax tree.
data Expr
  = ExprEmptySet
  | ExprNumeric Natural Char
  | ExprSetLiteral [Expr]
  | ExprMonad Char Expr
  | ExprDyad Char Expr Expr
  | ExprUniversalMonad Char Char Expr
  | ExprUniversalDyad Char Char Expr Expr
  deriving (Eq, Show)

data Stmt
  = StmtExpr Expr
  | StmtPrint Expr
  | StmtUniversalPrint Char Expr

type Parser = Parsec Void String

commitOn :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
commitOn f p q = liftA2 f (try $ p <* lookAhead q) q

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment [G.comment]) empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

universe :: Parser Char
universe = oneOf G.double

emptySet :: Parser Expr
emptySet = lexeme $ char G.emptySet $> ExprEmptySet

setLiteral :: Parser Expr
setLiteral = lexeme $ between (lexeme $ char G.setOpen) (lexeme $ char G.setClose) (ExprSetLiteral <$> sepBy expression (lexeme $ char G.elementSeparator))

numericLiteral :: Parser Expr
numericLiteral = lexeme (commitOn (ExprNumeric . read) (some digitChar) universe <?> "numeric literal")

term :: Parser Expr
term = emptySet <|> setLiteral <|> numericLiteral <|> between (lexeme $ char G.groupLeft) (lexeme $ char G.groupRight) expression

expression :: Parser Expr
expression = (*>) spaceConsumer $ lexeme $ makeExprParser term
  [ [ monad G.count ]
  , [ dyadUL G.cartesianProduct ]
  , [ dyadUL G.plus ]
  , [ dyadL G.difference ]
  , [ dyadL G.intersection ]
  , [ dyadL G.union ]
  , [ dyadL G.cartesianProduct ]
  , [ dyadN G.subset, dyadN G.superset, dyadN G.element, dyadN G.contains ]
  , [ dyadN G.pair ]
  ]
  where
    monad c = Prefix $ foldr1 (.) <$> some (lexeme $ char c $> ExprMonad c)
    dyadL c = InfixL $ lexeme $ char c $> ExprDyad c
    dyadN c = InfixN $ lexeme $ char c $> ExprDyad c
    dyadUL c = InfixL $ lexeme $ (commitOn ExprUniversalDyad (char c) universe <?> [c] ++ "universe")

universalPrintStatement :: Parser Stmt
universalPrintStatement = lexeme $ commitOn (StmtUniversalPrint .: flip const) (string "print") universe <*> expression

printStatement :: Parser Stmt
printStatement = lexeme $ string "print" $> StmtPrint <*> expression

statement :: Parser Stmt
statement = universalPrintStatement <|> printStatement <|> (StmtExpr <$> expression)

-- |  Parse code into an 'Expr'.
parse :: FilePath -> String -> Context Stmt
parse file source = let
  prettyError :: SourcePos -> String
  prettyError pos = let
    ls = lines source
    line = subtract 1 $ unPos $ sourceLine pos
    column = subtract 1 $ unPos $ sourceColumn pos
    theLine = if length ls <= line then "" else ls !! line
    in sourceName pos ++ ":" ++ show (unPos $ sourceLine pos) ++ ":" ++ show (unPos $ sourceColumn pos) ++ "\n" ++ theLine ++ "\n" ++ replicate column ' ' ++ "^\n"

  prettyParseError :: SourcePos -> ParseError String Void -> String
  prettyParseError pos err = prettyError pos ++ parseErrorTextPretty err

  makeParseErrors :: ParseErrorBundle String Void -> String
  makeParseErrors es = case attachSourcePos errorOffset (bundleErrors es) (bundlePosState es) of
    (r :| rs, _) -> concatMap (uncurry $ flip prettyParseError) $ r : rs
  in case first makeParseErrors $ Text.Megaparsec.parse (statement <* eof) file source of
    Left err -> throwError err
    Right s -> pure s
