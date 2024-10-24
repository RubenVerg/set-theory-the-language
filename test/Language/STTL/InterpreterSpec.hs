module Language.STTL.InterpreterSpec where

import Test.Hspec

import Language.STTL.Constructs
import Language.STTL.Interpreter
import Language.STTL.Parser
import Language.STTL.Set

import Data.Either

spec :: Spec
spec = do
  let r = run "test"
      i = interpret

  it "should evaluate empty sets" $ do
    r "∅" `shouldBe` pure emptySet

  it "should evaluate set literals" $ do
    r "{∅}" `shouldBe` pure (makeSet [emptySet])

  it "should evaluate monads" $ do
    r "#∅" `shouldBe` pure (makeNatural 0)
    r "#{∅}" `shouldBe` pure (makeNatural 1)

  it "should evaluate dyads" $ do
    r "{∅} ∪ ∅" `shouldBe` pure (setSingleton emptySet)
    r "{∅} ∩ ∅" `shouldBe` pure emptySet
    r "{∅} ∖ ∅" `shouldBe` pure (setSingleton emptySet)

  it "should fail with invalid inputs" $ do
    i (BranchMonad '?' LeafEmptySet) `shouldSatisfy` isLeft
    i (BranchDyad '?' LeafEmptySet LeafEmptySet) `shouldSatisfy` isLeft
