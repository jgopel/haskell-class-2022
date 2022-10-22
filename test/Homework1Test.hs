module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Homework1

main :: IO ()
main = do
  defaultMain (testGroup "Homework 1 tests" [
    toDigitsTest,
    toDigitsRevTest,
    doubleEveryOtherTest,
    sumDigitsTest,
    validateTest])

-- basics: https://github.com/Originate/guide/blob/master/haskell/stack-tutorial.md

toDigitsTest :: TestTree
toDigitsTest = testGroup "toDigits" [
  testCase "zero" $ assertEqual "should split zero to empty list" [] (toDigits 0),
  testCase "one digit" $ assertEqual "should split single digit number" [1] (toDigits 1),
  testCase "many digits" $ assertEqual "should split two digit number" [1, 2] (toDigits 12),
  testCase "provided example" $ assertEqual "should work on provided example" [1, 2, 3, 4] (toDigits 1234),
  testCase "negative number" $ assertEqual "should give empty list for negative numbers" [] (toDigits (-1))]


toDigitsRevTest :: TestTree
toDigitsRevTest = testGroup "toDigitsRev" [
  testCase "zero" $ assertEqual "should split zero to empty list" [] (toDigitsRev 0),
  testCase "one digit" $ assertEqual "should split single digit number" [1] (toDigitsRev 1),
  testCase "many digits" $ assertEqual "should split two digit number" [2, 1] (toDigitsRev 12),
  testCase "provided example" $ assertEqual "should work on provided example" [4, 3, 2, 1] (toDigitsRev 1234),
  testCase "negative number" $ assertEqual "should give empty list for negative numbers" [] (toDigitsRev (-1))]

doubleEveryOtherTest :: TestTree
doubleEveryOtherTest = testGroup "doubleEveryOther" [
  testCase "zero integers" $ assertEqual "should handle empty list" [] (doubleEveryOther []),
  testCase "one integer" $ assertEqual "should handle list with single element" [2] (doubleEveryOther [1]),
  testCase "many integers" $ assertEqual "should handle list with multiple elements" [2, 2, 6, 4] (doubleEveryOther [1, 2, 3, 4]),
  testCase "provided example" $ assertEqual "should work on provided example" [16, 7, 12, 5] (doubleEveryOther [8, 7, 6, 5])]

sumDigitsTest :: TestTree
sumDigitsTest = testGroup "sumDigits" [
  testCase "zero digits" $ assertEqual "should handle empty list" 0 (sumDigits []),
  testCase "one single digit digit" $ assertEqual "should handle single digit numbers" 1 (sumDigits [1]),
  testCase "one multi digit digit" $ assertEqual "should handle single digit numbers" 6 (sumDigits [123]),
  testCase "multiple digits" $ assertEqual "should handle multiple elements" 6 (sumDigits [12, 3]),
  testCase "provided example" $ assertEqual "should handle provided example" 22 (sumDigits [16, 7, 12, 5])]

validateTest :: TestTree
validateTest = testGroup "validate" [
  testCase "valid example" $ assertEqual "should handle valid example" True (validate 4012888888881881),
  testCase "invalid example" $ assertEqual "should handle invalid example" False (validate 4012888888881882)]
