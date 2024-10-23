module Language.STTL.SetSpec where

import Test.Hspec

import Language.STTL.Set

spec :: Spec
spec = do
  describe "makeSet" $ do
    it "should remove duplicates" $ do
      unSet (makeSet [emptySet, emptySet]) `shouldBe` unSet (makeSet [emptySet])
  
  describe "unSet" $ do
    it "should return contents sorted" $ do
      unSet (makeSet [setSingleton emptySet, emptySet]) `shouldBe` [emptySet, setSingleton emptySet]

  describe "emptySet" $ do
    it "should contain no elements" $ do
      unSet emptySet `shouldBe` []

  describe "setSingleton" $ do
    it "should wrap a value in a set" $ do
      unSet (setSingleton emptySet) `shouldBe` [emptySet]

  describe "setCount" $ do
    it "should return the number of elements in a set" $ do
      setCount emptySet `shouldBe` 0
      setCount (setSingleton emptySet) `shouldBe` 1
      setCount (makeSet [emptySet, setSingleton emptySet]) `shouldBe` 2

  describe "setUnion" $ do
    it "should return the union of two sets" $ do
      setUnion emptySet emptySet `shouldBe` emptySet
      setUnion emptySet (setSingleton emptySet) `shouldBe` setSingleton emptySet
      setUnion (setSingleton emptySet) (setSingleton emptySet) `shouldBe` setSingleton emptySet
      setUnion (makeSet [emptySet, setSingleton emptySet]) (makeSet [emptySet, setSingleton (setSingleton emptySet)])
        `shouldBe` makeSet [emptySet, setSingleton emptySet, setSingleton (setSingleton emptySet)]
  
  describe "setIntersection" $ do
    it "should return the intersection of two sets" $ do
      setIntersection emptySet emptySet `shouldBe` emptySet
      setIntersection emptySet (setSingleton emptySet) `shouldBe` emptySet
      setIntersection (setSingleton emptySet) (setSingleton emptySet) `shouldBe` setSingleton emptySet
      setIntersection (makeSet [emptySet, setSingleton emptySet]) (makeSet [emptySet, setSingleton (setSingleton emptySet)])
        `shouldBe` makeSet [emptySet]

  describe "setDifference" $ do
    it "should return the difference of two sets" $ do
      setDifference emptySet emptySet `shouldBe` emptySet
      setDifference emptySet (setSingleton emptySet) `shouldBe` emptySet
      setDifference (setSingleton emptySet) (setSingleton emptySet) `shouldBe` emptySet
      setDifference (setSingleton emptySet) emptySet `shouldBe` setSingleton emptySet
      setDifference (makeSet [emptySet, setSingleton emptySet]) (makeSet [emptySet, setSingleton (setSingleton emptySet)])
        `shouldBe` makeSet [setSingleton emptySet]

  describe "instance Eq Set" $ do
    it "should compare equal sets equal" $ do
      setSingleton emptySet `shouldBe` setSingleton emptySet
      emptySet `shouldBe` emptySet
      makeSet [emptySet, setSingleton emptySet] `shouldBe` makeSet [emptySet, setSingleton emptySet]
      makeSet [setSingleton emptySet, emptySet] `shouldBe` makeSet [emptySet, setSingleton emptySet]
    it "should compare different sets unequal" $ do
      emptySet `shouldNotBe` setSingleton emptySet

  describe "instance Ord Set" $ do
    it "should return EQ for equal sets" $ do
      emptySet `compare` emptySet `shouldBe` EQ
      makeSet [emptySet, setSingleton emptySet] `compare` makeSet [emptySet, setSingleton emptySet] `shouldBe` EQ
      makeSet [setSingleton emptySet, emptySet] `compare` makeSet [emptySet, setSingleton emptySet] `shouldBe` EQ
    it "should compare different sets lexicographically" $ do
      emptySet `compare` setSingleton emptySet `shouldBe` LT
      setSingleton emptySet `compare` emptySet `shouldBe` GT
      makeSet [emptySet, setSingleton emptySet] `compare` makeSet [emptySet] `shouldBe` GT
      makeSet [emptySet, setSingleton emptySet] `compare` makeSet [setSingleton emptySet] `shouldBe` LT

  describe "instance Show Set" $ do
    it "should show the empty set" $ do
      show emptySet `shouldBe` "∅"
    it "should show a singleton set" $ do
      show (setSingleton emptySet) `shouldBe` "{∅}"
    it "should show a set with multiple elements" $ do
      show (makeSet [emptySet, setSingleton emptySet]) `shouldBe` "{∅, {∅}}"
    
  