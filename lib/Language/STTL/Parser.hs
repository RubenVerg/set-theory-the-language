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

-- | Abstract syntax tree.
data AST
  = LeafEmptySet
  | BranchSetLiteral [AST]
  | BranchMonad Char AST
  | BranchDyad Char AST AST
  deriving (Eq, Show)

type Parser = Parsec Void String

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment [G.comment]) empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

emptySet :: Parser AST
emptySet = lexeme $ char G.emptySet $> LeafEmptySet

setLiteral :: Parser AST
setLiteral = lexeme $ between (lexeme $ char G.setOpen) (lexeme $ char G.setClose) (BranchSetLiteral <$> sepBy expression (lexeme $ char G.elementSeparator))

term :: Parser AST
term = emptySet <|> setLiteral <|> between (lexeme $ char G.groupLeft) (lexeme $ char G.groupRight) expression

expression :: Parser AST
expression = (*>) spaceConsumer $ lexeme $ makeExprParser term
  [ [ monad G.count ]
  , [ dyad G.difference ]
  , [ dyad G.intersection ]
  , [ dyad G.union ]
  ]
  where
    monad c = Prefix $ foldr1 (.) <$> some (lexeme $ char c $> BranchMonad c)
    dyad c = InfixL $ lexeme $ char c $> BranchDyad c

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
