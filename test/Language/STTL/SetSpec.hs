{-# LANGUAGE OverloadedLists #-}
module Language.STTL.SetSpec where

import Test.Hspec

import Language.STTL.Constructs
import Language.STTL.Set

spec :: Spec
spec = do
  describe "makeSet" $ do
    it "should remove duplicates" $ do
      unSet [[], []] `shouldBe` unSet [[]]
  
  describe "unSet" $ do
    it "should return contents sorted" $ do
      unSet [[[]], []] `shouldBe` [[], [[]]]

  describe "emptySet" $ do
    it "should contain no elements" $ do
      unSet emptySet `shouldBe` []

  describe "setSingleton" $ do
    it "should wrap a value in a set" $ do
      unSet (setSingleton emptySet) `shouldBe` [emptySet]

  describe "setCount" $ do
    it "should return the number of elements in a set" $ do
      setCount [] `shouldBe` 0
      setCount [[]] `shouldBe` 1
      setCount [[], [[]]] `shouldBe` 2

  describe "setUnion" $ do
    it "should return the union of two sets" $ do
      setUnion [] [] `shouldBe` []
      setUnion [] [[]] `shouldBe` [[]]
      setUnion [[]] [[]] `shouldBe` [[]]
      setUnion [[], [[]]] [[], [[[]]]] `shouldBe` [[], [[]], [[[]]]]
  
  describe "setIntersection" $ do
    it "should return the intersection of two sets" $ do
      setIntersection [] [] `shouldBe` []
      setIntersection [] [[]] `shouldBe` []
      setIntersection [[]] [[]] `shouldBe` [[]]
      setIntersection [[], [[]]] [[], [[[]]]] `shouldBe` [[]]

  describe "setDifference" $ do
    it "should return the difference of two sets" $ do
      setDifference [] [] `shouldBe` []
      setDifference [] [[]] `shouldBe` []
      setDifference [[]] [[]] `shouldBe` []
      setDifference [[]] [] `shouldBe` [[]]
      setDifference [[], [[]]] [[], [[[]]]] `shouldBe` [[[]]]

  describe "setSubset" $ do
    it "should check whether a set is a subset of another" $ do
      setSubset [[]] (makeNatural 5) `shouldBe` True
      setSubset [[]] [] `shouldBe` False
    it "should say that the empty set is a subset of all sets" $ do
      setSubset [] (makeNatural 5) `shouldBe` True

  describe "setSuperset" $ do
    it "should check whether a set is a superset of another" $ do
      setSuperset (makeNatural 5) [[]] `shouldBe` True
      setSuperset [] [[]] `shouldBe` False
    it "should say that all sets are supersets of the empty set" $ do
      setSuperset (makeNatural 5) [] `shouldBe` True

  describe "setElement" $ do
    it "should check whether a set contains an element" $ do
      setElement [[]] [] `shouldBe` False
      setElement [[]] (makeNatural 5) `shouldBe` True

  describe "setContains" $ do
    it "should check whether a set contains another" $ do
      setContains [] [[]] `shouldBe` False
      setContains (makeNatural 5) [[]] `shouldBe` True

  describe "instance Eq Set" $ do
    it "should compare equal sets equal" $ do
      Set [[]] `shouldBe` Set [[]]
      Set [] `shouldBe` Set []
      Set [[], [[]]] `shouldBe` Set [[], [[]]]
      Set [[[]], []] `shouldBe` Set [[], [[]]]
    it "should compare different sets unequal" $ do
      Set [] `shouldNotBe` Set [[]]

  describe "instance Ord Set" $ do
    it "should return EQ for equal sets" $ do
      Set [] `compare` Set [] `shouldBe` EQ
      Set [[], [[]]] `compare` Set [[], [[]]] `shouldBe` EQ
      Set [[[]], []] `compare` Set [[], [[]]] `shouldBe` EQ
    it "should compare different sets lexicographically" $ do
      Set [] `compare` Set [[]] `shouldBe` LT
      Set [[]] `compare` Set [] `shouldBe` GT
      Set [[], [[]]] `compare` Set [[]] `shouldBe` GT
      Set [[], [[]]] `compare` Set [[[]]] `shouldBe` LT

  describe "instance Show Set" $ do
    it "should show the empty set" $ do
      show emptySet `shouldBe` "∅"
    it "should show a singleton set" $ do
      show (Set [emptySet]) `shouldBe` "{∅}"
    it "should show a set with multiple elements" $ do
      show (Set [emptySet, [emptySet]]) `shouldBe` "{∅, {∅}}"
    
  