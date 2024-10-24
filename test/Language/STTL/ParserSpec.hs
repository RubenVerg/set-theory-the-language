module Language.STTL.ParserSpec where

import Test.Hspec

import Language.STTL.Parser
import Language.STTL.Context

spec :: Spec
spec = do
  let p = runContext . parse "test"
  let pe code = do {
      res <- p code
    ; case res of {
        Left err -> pure $ Left err
      ; Right (StmtExpr e) -> pure $ Right e
      ; Right _ -> pure $ Left "Expected expression" } }

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

    it "should parse number literals" $ do
      pe "1234𝕒" `shouldReturn` pure (ExprNumeric 1234 '𝕒')
      pe "0𝕒" `shouldReturn` pure (ExprNumeric 0 '𝕒')

