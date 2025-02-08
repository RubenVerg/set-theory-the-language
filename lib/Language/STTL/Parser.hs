-- | Module: Language.STTL.Parser
--
-- Parser for Set Theory: The Language.
module Language.STTL.Parser
  ( Expr(..)
  , Stmt(..)
  , parseExpr
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
import Data.Composition

type UniverseId = Char

-- | Expression abstract syntax tree.
data Expr
  = ExprEmptySet
  | ExprNumeric Integer UniverseId
  | ExprGet
  | ExprUniversalGet UniverseId
  | ExprSetLiteral [Expr]
  | ExprMonad Char Expr
  | ExprDyad Char Expr Expr
  | ExprUniversalMonad Char UniverseId Expr
  | ExprUniversalDyad Char UniverseId Expr Expr
  | ExprBiversalMonad Char UniverseId UniverseId Expr
  deriving (Eq, Show)

-- | Statement abstract syntax tree.
data Stmt
  = StmtExpr Expr
  | StmtPrint Expr
  | StmtUniversalPrint Char Expr
  deriving (Eq, Show)

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
numericLiteral = lexeme (commitOn (ExprNumeric) (do
  sign <- choice [char G.highMinus $> (-1), pure 1]
  digs <- some digitChar
  pure $ sign * read digs) universe <?> "numeric literal")

universalGet :: Parser Expr
universalGet = lexeme (commitOn (ExprUniversalGet .: flip const) (string "get") universe <?> "get𝕦")

get :: Parser Expr
get = lexeme $ string "get" $> ExprGet

term :: Parser Expr
term = emptySet <|> setLiteral <|> numericLiteral <|> universalGet <|> get <|> between (lexeme $ char G.groupLeft) (lexeme $ char G.groupRight) expression

expression :: Parser Expr
expression = (*>) spaceConsumer $ lexeme $ makeExprParser term
  [ [ monads [ monad G.count, monadUL G.negation ] ]
  , [ dyadUL G.cartesianProduct ]
  , [ dyadUL G.plus, dyadUL G.minus ]
  , [ dyadL G.difference ]
  , [ dyadL G.intersection ]
  , [ dyadL G.union ]
  , [ dyadL G.cartesianProduct ]
  , [ dyadN G.subset, dyadN G.superset, dyadN G.element, dyadN G.contains ]
  , [ dyadN G.pair ]
  , [ monads [ monadUUL G.convert, monadUUL G.inject ] ]
  ]
  where
    monad c = lexeme $ char c $> ExprMonad c
    dyadL c = InfixL $ lexeme $ char c $> ExprDyad c
    dyadN c = InfixN $ lexeme $ char c $> ExprDyad c
    dyadUL c = InfixL $ lexeme $ (commitOn ExprUniversalDyad (char c) universe <?> [c] ++ "𝕦")
    monadUL c = lexeme $ (commitOn ExprUniversalMonad (char c) universe <?> [c] ++ "𝕦")
    monadUUL c = lexeme $ (commitOn (\_ (u, v) -> ExprBiversalMonad c u v) (char c) (commitOn (,) universe universe) <?> [c] ++ "𝕦𝕧")
    monads cs = Prefix $ foldr1 (.) <$> some (choice cs)

universalPrintStatement :: Parser Stmt
universalPrintStatement = lexeme (commitOn (StmtUniversalPrint .: flip const) (string "print") universe <*> expression <?> "print𝕦")

printStatement :: Parser Stmt
printStatement = lexeme $ string "print" $> StmtPrint <*> expression

statement :: Parser Stmt
statement = universalPrintStatement <|> printStatement <|> (StmtExpr <$> expression)

prettyError :: String -> SourcePos -> String
prettyError source pos = let
  ls = lines source
  line = subtract 1 $ unPos $ sourceLine pos
  column = subtract 1 $ unPos $ sourceColumn pos
  theLine = if length ls <= line then "" else ls !! line
  in sourceName pos ++ ":" ++ show (unPos $ sourceLine pos) ++ ":" ++ show (unPos $ sourceColumn pos) ++ "\n" ++ theLine ++ "\n" ++ replicate column ' ' ++ "^\n"

prettyParseError :: String -> SourcePos -> ParseError String Void -> String
prettyParseError source pos err = prettyError source pos ++ parseErrorTextPretty err

makeParseErrors :: String -> ParseErrorBundle String Void -> String
makeParseErrors source es = case attachSourcePos errorOffset (bundleErrors es) (bundlePosState es) of
  (r :| rs, _) -> concatMap (uncurry $ flip (prettyParseError source)) $ r : rs

-- | Parse code representing an expression into an 'Expr'.
parseExpr :: FilePath -> String -> Context Expr
parseExpr file source = case first (makeParseErrors source) $ Text.Megaparsec.parse (expression <* eof) file source of
  Left err -> throwError err
  Right s -> pure s

-- | Parse code into a 'Stmt'.
parse :: FilePath -> String -> Context Stmt
parse file source = case first (makeParseErrors source) $ Text.Megaparsec.parse (statement <* eof) file source of
  Left err -> throwError err
  Right s -> pure s
