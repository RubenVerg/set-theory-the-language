module Language.STTL.ParserSpec where

import Test.Hspec

import Language.STTL.Parser
import Language.STTL.Context

spec :: Spec
spec = do
  let p = runContext . parse "test"
  let pe = runContext . parseExpr "text"

  describe "expressions" $ do
    it "shoud parse empty sets" $ do
      pe "∅" `shouldReturn` pure ExprEmptySet

    it "should parse set notation" $ do
      pe "{}" `shouldReturn` pure (ExprSetLiteral [])
      pe "{∅}" `shouldReturn` pure (ExprSetLiteral [ExprEmptySet])
      pe "{∅, ∅}" `shouldReturn` pure (ExprSetLiteral [ExprEmptySet, ExprEmptySet])

    it "should parse operators" $ do
      pe "#∅" `shouldReturn` pure (ExprMonad '#' ExprEmptySet)
      pe "∅ ∪ ∅" `shouldReturn` pure (ExprDyad '∪' ExprEmptySet ExprEmptySet)
      pe "∅ ∩ ∅" `shouldReturn` pure (ExprDyad '∩' ExprEmptySet ExprEmptySet)
      pe "∅ ∖ ∅" `shouldReturn` pure (ExprDyad '∖' ExprEmptySet ExprEmptySet)
      pe "∅ × ∅" `shouldReturn` pure (ExprDyad '×' ExprEmptySet ExprEmptySet)
      pe "∅ ⊆ ∅" `shouldReturn` pure (ExprDyad '⊆' ExprEmptySet ExprEmptySet)
      pe "∅ ⊇ ∅" `shouldReturn` pure (ExprDyad '⊇' ExprEmptySet ExprEmptySet)
      pe "∅ ∈ ∅" `shouldReturn` pure (ExprDyad '∈' ExprEmptySet ExprEmptySet)
      pe "∅ ∋ ∅" `shouldReturn` pure (ExprDyad '∋' ExprEmptySet ExprEmptySet)
      pe "∅ ; ∅" `shouldReturn` pure (ExprDyad ';' ExprEmptySet ExprEmptySet)

    it "should respect operator precedence" $ do
      pe "∅ ∪ ∅ ∩ ∅" `shouldReturn` pure (ExprDyad '∪' ExprEmptySet (ExprDyad '∩' ExprEmptySet ExprEmptySet))
      pe "∅ ∖ ∅ ∩ ∅" `shouldReturn` pure (ExprDyad '∩' (ExprDyad '∖' ExprEmptySet ExprEmptySet) ExprEmptySet)
      pe "∅ ∖ ∅ ∪ ∅" `shouldReturn` pure (ExprDyad '∪' (ExprDyad '∖' ExprEmptySet ExprEmptySet) ExprEmptySet)

    it "should allow double monads" $ do
      pe "##∅" `shouldReturn` pure (ExprMonad '#' (ExprMonad '#' ExprEmptySet))

    it "should parse parentheses" $ do
      pe "(∅)" `shouldReturn` pure ExprEmptySet

    it "should parse universal operators" $ do
      pe "∅ +𝕒 ∅" `shouldReturn` pure (ExprUniversalDyad '+' '𝕒' ExprEmptySet ExprEmptySet)
      pe "∅ ×𝕒 ∅" `shouldReturn` pure (ExprUniversalDyad '×' '𝕒' ExprEmptySet ExprEmptySet)
      pe "∅ -𝕒 ∅" `shouldReturn` pure (ExprUniversalDyad '-' '𝕒' ExprEmptySet ExprEmptySet)

    it "should parse number literals" $ do
      pe "1234𝕒" `shouldReturn` pure (ExprNumeric 1234 '𝕒')
      pe "0𝕒" `shouldReturn` pure (ExprNumeric 0 '𝕒')
      pe "¯5𝕒" `shouldReturn` pure (ExprNumeric (-5) '𝕒')
    
    it "should parse get" $ do
      pe "get" `shouldReturn` pure ExprGet

    it "should parse universal get" $ do
      pe "get𝕒" `shouldReturn` pure (ExprUniversalGet '𝕒')

  describe "statements" $ do
    it "should parse expression statements" $ do
      p "∅∪∅" `shouldReturn` pure (StmtExpr (ExprDyad '∪' ExprEmptySet ExprEmptySet))

    it "should parse print statements" $ do
      p "print∅" `shouldReturn` pure (StmtPrint ExprEmptySet)

    it "should parse universal print statements" $ do
      p "print𝕒∅" `shouldReturn` pure (StmtUniversalPrint '𝕒' ExprEmptySet)

