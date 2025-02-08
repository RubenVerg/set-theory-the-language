{-# LANGUAGE OverloadedLists #-}
module Language.STTL.ConstructsSpec where

import Test.Hspec

import Language.STTL.Constructs
import Language.STTL.Set

spec :: Spec
spec = do
  describe "pairs" $ do
    describe "makePair" $ do
      it "should make pairs" $ do
        makePair ([], [[]]) `shouldBe` [[[]], [[], [[]]]]
        makePair ([[]], []) `shouldBe` [[[[]]], [[[]], []]]
        makePair ([], []) `shouldBe` [[[]]]
    
    describe "unPair" $ do
      it "should return the contents of a pair" $ do
        unPair (makePair ([], [[]])) `shouldBe` Just ([], [[]])
        unPair (makePair ([[]], [])) `shouldBe` Just ([[]], [])
        unPair (makePair ([], [])) `shouldBe` Just ([], [])
        unPair [] `shouldBe` Nothing
        unPair [[], [[]], [[[]]]] `shouldBe` Nothing
        unPair [[], [[]]] `shouldBe` Nothing
        unPair [[[]], [[[[]]], [[[[]]]]]] `shouldBe` Nothing

    describe "isPair" $ do
      it "should return True for pairs" $ do
        isPair (makePair ([], [[]])) `shouldBe` True
        isPair (makePair ([[]], [])) `shouldBe` True
        isPair (makePair ([], [])) `shouldBe` True
      it "should return False for non-pairs" $ do
        isPair [] `shouldBe` False
        isPair [[], [[]], [[[]]]] `shouldBe` False

    describe "pairFst" $ do
      it "should return the first element of a pair" $ do
        pairFst (makePair ([], [[]])) `shouldBe` Just []
        pairFst (makePair ([[]], [])) `shouldBe` Just [[]]
        pairFst (makePair ([], [])) `shouldBe` Just []
        pairFst [] `shouldBe` Nothing

    describe "pairSnd" $ do
      it "should return the second element of a pair" $ do
        pairSnd (makePair ([], [[]])) `shouldBe` Just [[]]
        pairSnd (makePair ([[]], [])) `shouldBe` Just []
        pairSnd (makePair ([], [])) `shouldBe` Just []
        pairSnd [] `shouldBe` Nothing

    describe "cartesianProduct" $ do
      it "should return the cartesian product of two sets" $ do
        cartesianProduct (makeNatural 1) (makeNatural 2) `shouldBe` [makePair ([], []), makePair ([], [[]])]

  describe "naturals" $ do
    describe "naturalZero" $ do
      it "should be the empty set" $ do
        naturalZero `shouldBe` []

    describe "naturalSucc" $ do
      it "should return the successor of a natural number" $ do
        naturalSucc naturalZero `shouldBe` [[]]
        naturalSucc (makeNatural 7) `shouldBe` makeSet (makeNatural 7 : unSet (makeNatural 7))

    describe "makeNatural" $ do
      it "should make a natural number" $ do
        makeNatural 0 `shouldBe` naturalZero
        makeNatural 1 `shouldBe` [[]]
        makeNatural 2 `shouldBe` setUnion (makeNatural 1) [makeNatural 1]
        makeNatural 3 `shouldBe` setUnion (makeNatural 2) [makeNatural 2]
        makeNatural 10 `shouldBe` setUnion (makeNatural 9) [makeNatural 9]

    describe "unNatural" $ do
      it "should return the value of a natural number" $ do
        unNatural naturalZero `shouldBe` 0
        unNatural (makeNatural 1) `shouldBe` 1
        unNatural (makeNatural 5) `shouldBe` 5

  describe "integers" $ do
    describe "makeInteger" $ do
      it "should make an integer" $ do
        makeInteger 2 `shouldBe` makePair (makeNatural 2, makeNatural 0)
        makeInteger (-2) `shouldBe` makePair (makeNatural 0, makeNatural 2)
        makeInteger 0 `shouldBe` makePair (makeNatural 0, makeNatural 0)

    describe "unInteger" $ do
      it "should return the value of an integer" $ do
        unInteger (makeInteger 0) `shouldBe` Just 0
        unInteger (makeInteger 10) `shouldBe` Just 10
        unInteger (makeInteger (-7)) `shouldBe` Just (-7)

  describe "booleans" $ do
    describe "booleanFalse" $ do
      it "should be the empty set" $ do
        booleanFalse `shouldBe` []

    describe "booleanTrue" $ do
      it "should be a singleton of the empty set" $ do
        booleanTrue `shouldBe` [[]]

    describe "makeBoolean" $ do
      it "should make a boolean" $ do
        makeBoolean False `shouldBe` booleanFalse
        makeBoolean True `shouldBe` booleanTrue

    describe "unBoolean" $ do
      it "turns empty sets into False" $ do
        unBoolean [] `shouldBe` False
      it "turns all other sets into True" $ do
        unBoolean booleanTrue `shouldBe` True
        unBoolean (makeNatural 4) `shouldBe` True
