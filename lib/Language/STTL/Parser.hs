-- | Module: Language.STTL.Parser
--
-- Parser for Set Theory: The Language.
module Language.STTL.Parser
  ( AST(..)
  , Language.STTL.Parser.parse
  ) where

import qualified Language.STTL.Glyphs as G

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Functor
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Bifunctor
import Control.Monad.Combinators.Expr
import Numeric.Natural

-- | Abstract syntax tree.
data AST
  = LeafEmptySet
  | LeafNumeric Natural Char
  | BranchSetLiteral [AST]
  | BranchMonad Char AST
  | BranchDyad Char AST AST
  | BranchUniversalMonad Char Char AST
  | BranchUniversalDyad Char Char AST AST
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

emptySet :: Parser AST
emptySet = lexeme $ char G.emptySet $> LeafEmptySet

setLiteral :: Parser AST
setLiteral = lexeme $ between (lexeme $ char G.setOpen) (lexeme $ char G.setClose) (BranchSetLiteral <$> sepBy expression (lexeme $ char G.elementSeparator))

numericLiteral :: Parser AST
numericLiteral = lexeme (commitOn (LeafNumeric . read) (some digitChar) universe <?> "numeric literal")

term :: Parser AST
term = emptySet <|> setLiteral <|> numericLiteral <|> between (lexeme $ char G.groupLeft) (lexeme $ char G.groupRight) expression

expression :: Parser AST
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
    monad c = Prefix $ foldr1 (.) <$> some (lexeme $ char c $> BranchMonad c)
    dyadL c = InfixL $ lexeme $ char c $> BranchDyad c
    dyadN c = InfixN $ lexeme $ char c $> BranchDyad c
    dyadUL c = InfixL $ lexeme $ (commitOn BranchUniversalDyad (char c) universe <?> [c] ++ "universe")

-- |  Parse code into an 'AST'.
parse :: FilePath -> String -> Either String AST
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
  in first makeParseErrors $ Text.Megaparsec.parse (expression <* eof) file source
