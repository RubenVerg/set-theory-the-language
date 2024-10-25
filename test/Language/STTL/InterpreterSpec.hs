module Language.STTL.InterpreterSpec where

import Test.Hspec

import Language.STTL.Constructs
import Language.STTL.Interpreter
import Language.STTL.Parser
import Language.STTL.Set
import Language.STTL.Context

import Data.Either

spec :: Spec
spec = do
  let r = runContext . run "test"
      i = runContext . interpret

  it "should evaluate empty sets" $ do
    r "∅" `shouldReturn` pure (Just emptySet)

  it "should evaluate set literals" $ do
    r "{∅}" `shouldReturn` pure (Just $ makeSet [emptySet])

  it "should evaluate monads" $ do
    r "#∅" `shouldReturn` pure (Just $ makeNatural 0)
    r "#{∅}" `shouldReturn` pure (Just $ makeNatural 1)

  it "should evaluate dyads" $ do
    r "{∅} ∪ ∅" `shouldReturn` pure (Just $ setSingleton emptySet)
    r "{∅} ∩ ∅" `shouldReturn` pure (Just emptySet)
    r "{∅} ∖ ∅" `shouldReturn` pure (Just $ setSingleton emptySet)
    r "{∅} × {∅}" `shouldReturn` pure (Just $ setSingleton $ makePair (emptySet, emptySet))
    r "∅ ⊆ {∅}" `shouldReturn` pure (Just booleanTrue)
    r "∅ ⊇ {∅}" `shouldReturn` pure (Just booleanFalse)
    r "∅ ∈ {∅}" `shouldReturn` pure (Just booleanTrue)
    r "∅ ∋ {∅}" `shouldReturn` pure (Just booleanFalse)
    r "∅ ; {∅}" `shouldReturn` pure (Just $ makePair (emptySet, setSingleton emptySet))

  it "should evaluate universal monads" $ do
    True `shouldBe` True

  it "should evaluate universal dyads" $ do
    r "∅ +ℕ {∅}" `shouldReturn` pure (Just $ makeNatural 1)
    r "{∅} ×ℕ {∅, {∅}}" `shouldReturn` pure (Just $ makeNatural 2)
    r "5ℤ -ℤ 0ℤ" `shouldReturn` pure (Just $ makeInteger 5)

  it "should evaluate biversal monads" $ do
    r "→ℕℤ 5ℕ" `shouldReturn` pure (Just $ makeInteger 5)
    r "→ℤℕ 5ℤ" `shouldReturn` pure (Just $ makeNatural 5)

  it "should evaluate numeric literals" $ do
    r "12ℕ" `shouldReturn` pure (Just $ makeNatural 12)
    r "¯5ℤ" `shouldReturn` pure (Just $ makeInteger (-5))

  it "should fail with invalid inputs" $ do
    i (StmtExpr $ ExprMonad '?' ExprEmptySet) >>= (`shouldSatisfy` isLeft)
    i (StmtExpr $ ExprDyad '?' ExprEmptySet ExprEmptySet) >>= (`shouldSatisfy` isLeft)
    i (StmtExpr $ ExprUniversalMonad '?' '?' ExprEmptySet) >>= (`shouldSatisfy` isLeft)
    i (StmtExpr $ ExprUniversalDyad '?' '?' ExprEmptySet ExprEmptySet) >>= (`shouldSatisfy` isLeft)
    i (StmtExpr $ ExprUniversalDyad '+' 'p' ExprEmptySet ExprEmptySet) >>= (`shouldSatisfy` isLeft)
