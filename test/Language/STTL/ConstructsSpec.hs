module Language.STTL.ConstructsSpec where

import Test.Hspec

import Language.STTL.Constructs
import Language.STTL.Set

spec :: Spec
spec = do
  describe "pairs" $ do
    describe "makePair" $ do
      it "should make pairs" $ do
        makePair (emptySet, setSingleton emptySet) `shouldBe` makeSet [setSingleton emptySet, makeSet [emptySet, setSingleton emptySet]]
        makePair (setSingleton emptySet, emptySet) `shouldBe` makeSet [setSingleton (setSingleton emptySet), makeSet [emptySet, setSingleton emptySet]]
        makePair (emptySet, emptySet) `shouldBe` setSingleton (setSingleton emptySet)
    
    describe "unPair" $ do
      it "should return the contents of a pair" $ do
        unPair (makePair (emptySet, setSingleton emptySet)) `shouldBe` Just (emptySet, setSingleton emptySet)
        unPair (makePair (setSingleton emptySet, emptySet)) `shouldBe` Just (setSingleton emptySet, emptySet)
        unPair (makePair (emptySet, emptySet)) `shouldBe` Just (emptySet, emptySet)
        unPair emptySet `shouldBe` Nothing
        unPair (makeSet [emptySet, setSingleton emptySet, setSingleton (setSingleton emptySet)]) `shouldBe` Nothing
        unPair (makeSet [emptySet, setSingleton emptySet]) `shouldBe` Nothing
        unPair (makeSet [setSingleton emptySet, makeSet [setSingleton (setSingleton emptySet), setSingleton (setSingleton (setSingleton emptySet))]]) `shouldBe` Nothing

    describe "isPair" $ do
      it "should return True for pairs" $ do
        isPair (makePair (emptySet, setSingleton emptySet)) `shouldBe` True
        isPair (makePair (setSingleton emptySet, emptySet)) `shouldBe` True
        isPair (makePair (emptySet, emptySet)) `shouldBe` True
      it "should return False for non-pairs" $ do
        isPair emptySet `shouldBe` False
        isPair (makeSet [emptySet, setSingleton emptySet, setSingleton (setSingleton emptySet)]) `shouldBe` False

    describe "pairFst" $ do
      it "should return the first element of a pair" $ do
        pairFst (makePair (emptySet, setSingleton emptySet)) `shouldBe` Just emptySet
        pairFst (makePair (setSingleton emptySet, emptySet)) `shouldBe` Just (setSingleton emptySet)
        pairFst (makePair (emptySet, emptySet)) `shouldBe` Just emptySet
        pairFst emptySet `shouldBe` Nothing

    describe "pairSnd" $ do
      it "should return the second element of a pair" $ do
        pairSnd (makePair (emptySet, setSingleton emptySet)) `shouldBe` Just (setSingleton emptySet)
        pairSnd (makePair (setSingleton emptySet, emptySet)) `shouldBe` Just emptySet
        pairSnd (makePair (emptySet, emptySet)) `shouldBe` Just emptySet
        pairSnd emptySet `shouldBe` Nothing

    describe "cartesianProduct" $ do
      it "should return the cartesian product of two sets" $ do
        cartesianProduct (makeNatural 1) (makeNatural 2) `shouldBe` makeSet [makePair (emptySet, emptySet), makePair (emptySet, setSingleton emptySet)]

  describe "naturals" $ do
    describe "naturalZero" $ do
      it "should be the empty set" $ do
        naturalZero `shouldBe` emptySet

    describe "naturalSucc" $ do
      it "should return the successor of a natural number" $ do
        naturalSucc naturalZero `shouldBe` setSingleton emptySet
        naturalSucc (makeNatural 7) `shouldBe` makeSet (makeNatural 7 : unSet (makeNatural 7))

    describe "makeNatural" $ do
      it "should make a natural number" $ do
        makeNatural 0 `shouldBe` naturalZero
        makeNatural 1 `shouldBe` setSingleton emptySet
        makeNatural 2 `shouldBe` setUnion (makeNatural 1) (setSingleton $ makeNatural 1)
        makeNatural 3 `shouldBe` setUnion (makeNatural 2) (setSingleton $ makeNatural 2)
        makeNatural 10 `shouldBe` setUnion (makeNatural 9) (setSingleton $ makeNatural 9)

    describe "unNatural" $ do
      it "should return the value of a natural number" $ do
        unNatural naturalZero `shouldBe` 0
        unNatural (makeNatural 1) `shouldBe` 1
        unNatural (makeNatural 5) `shouldBe` 5

  describe "booleans" $ do
    describe "booleanFalse" $ do
      it "should be the empty set" $ do
        booleanFalse `shouldBe` emptySet

    describe "booleanTrue" $ do
      it "should be a singleton of the empty set" $ do
        booleanTrue `shouldBe` setSingleton emptySet

    describe "makeBoolean" $ do
      it "should make a boolean" $ do
        makeBoolean False `shouldBe` booleanFalse
        makeBoolean True `shouldBe` booleanTrue

    describe "unBoolean" $ do
      it "turns empty sets into False" $ do
        unBoolean emptySet `shouldBe` False
      it "turns all other sets into True" $ do
        unBoolean booleanTrue `shouldBe` True
        unBoolean (makeNatural 4) `shouldBe` True
