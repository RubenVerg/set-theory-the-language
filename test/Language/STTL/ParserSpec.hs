module Language.STTL.ParserSpec where

import Test.Hspec

import Language.STTL.Parser

spec :: Spec
spec = do
  let p = parse "test"

  it "shoud parse empty sets" $ do
    p "∅" `shouldBe` pure LeafEmptySet

  it "should parse set notation" $ do
    p "{}" `shouldBe` pure (BranchSetLiteral [])
    p "{∅}" `shouldBe` pure (BranchSetLiteral [LeafEmptySet])
    p "{∅, ∅}" `shouldBe` pure (BranchSetLiteral [LeafEmptySet, LeafEmptySet])

  it "should parse operators" $ do
    p "#∅" `shouldBe` pure (BranchMonad '#' LeafEmptySet)
    p "∅ ∪ ∅" `shouldBe` pure (BranchDyad '∪' LeafEmptySet LeafEmptySet)
    p "∅ ∩ ∅" `shouldBe` pure (BranchDyad '∩' LeafEmptySet LeafEmptySet)
    p "∅ ∖ ∅" `shouldBe` pure (BranchDyad '∖' LeafEmptySet LeafEmptySet)

  it "should respect operator precedence" $ do
    p "∅ ∪ ∅ ∩ ∅" `shouldBe` pure (BranchDyad '∪' LeafEmptySet (BranchDyad '∩' LeafEmptySet LeafEmptySet))
    p "∅ ∖ ∅ ∩ ∅" `shouldBe` pure (BranchDyad '∩' (BranchDyad '∖' LeafEmptySet LeafEmptySet) LeafEmptySet)
    p "∅ ∖ ∅ ∪ ∅" `shouldBe` pure (BranchDyad '∪' (BranchDyad '∖' LeafEmptySet LeafEmptySet) LeafEmptySet)

  it "should allow double monads" $ do
    p "##∅" `shouldBe` pure (BranchMonad '#' (BranchMonad '#' LeafEmptySet))

  it "should parse parentheses" $ do
    p "(∅)" `shouldBe` pure LeafEmptySet
    
