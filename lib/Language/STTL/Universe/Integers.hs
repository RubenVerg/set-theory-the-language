-- |
-- Module: Language.STTL.Universe.Integers
--
-- The integers universe.
module Language.STTL.Universe.Integers
  ( integersChar
  , integersParseNumeric
  , integersShow
  , integersRead
  , integersPlus
  , integersMinus
  , integersTimes
  , integers
  ) where

import qualified Language.STTL.Glyphs as G
import Language.STTL.Context
import Language.STTL.Set
import Language.STTL.Constructs
import Language.STTL.Multiverse

import Text.Read
import Numeric.Natural

ui :: Set -> Context Integer
ui s = case unInteger s of
  Nothing -> throwError "Not an integer!"
  Just i -> pure i

up :: Set -> Context (Natural, Natural)
up s = case unPair s of
  Nothing -> throwError "Not an integer!"
  Just (a, b) -> pure (unNatural a, unNatural b)

-- | The character for the integers universe, @ℤ@.
integersChar :: Char
integersChar = 'ℤ'

-- | Turn an integer into a set.
integersParseNumeric :: Integer -> Context Set
integersParseNumeric = pure . makeInteger

-- | Representation of an integer.
integersShow :: Set -> Context String
integersShow s = map (\ch -> if ch == '-' then G.highMinus else ch) . show <$> ui s

-- | Parsing of an integer.
integersRead :: String -> Context Set
integersRead s = case readMaybe $ (\ch -> if ch == G.highMinus then '-' else ch) <$> s of
  Nothing -> throwError $ "Could not parse integer: " ++ s
  Just i -> pure $ makeInteger i

integersPlus :: Set -> Set -> Context Set
integersPlus a b = do
  (ap, an) <- up a
  (bp, bn) <- up b
  pure $ makePair (makeNatural $ ap + bp, makeNatural $ an + bn)

integersMinus :: Set -> Set -> Context Set
integersMinus a b = do
  (ap, an) <- up a
  (bp, bn) <- up b
  pure $ makePair (makeNatural $ ap + bn, makeNatural $ an + bp)

integersTimes :: Set -> Set -> Context Set
integersTimes a b = do
  (ap, an) <- up a
  (bp, bn) <- up b
  pure $ makePair (makeNatural $ ap * bp + an * bn, makeNatural $ ap * bn + an * bp)

integersNegation :: Set -> Context Set
integersNegation = integersMinus (makeInteger 0)

integers :: Universe
integers = Universe
  { universeChar = integersChar
  , universeParseNumeric = Just integersParseNumeric
  , universeShow = Just integersShow
  , universeRead = Just integersRead
  , universePlus = Just integersPlus
  , universeMinus = Just integersMinus
  , universeTimes = Just integersTimes
  , universeNegation = Just integersNegation
  , universeUnion = Nothing
  , universeIntersection = Nothing
  }
