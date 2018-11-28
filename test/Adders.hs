module Adders where

import Control.Arrow
import Test.HUnit

import Adder
import Utils

testAdder :: ((Int,Int,Int) -> (Int,Int)) -> String -> Assertion
testAdder a name = assertBool failMsg (actual == expected) where
    expected = sum . tupleToList3 <$> bits
    actual = (uncurry (+) . second (*2) . a) <$> bits
    failMsg = "Adder " ++ name ++ " failed. Expected "
        ++ (show expected) ++ ", got " ++ (show actual)
        ++ ".\nBits: " ++ (show bits)

testAdder1 :: Assertion
testAdder1 = testAdder adder "1"

testAdder2 :: Assertion
testAdder2 = testAdder adder' "2"

testAdder3 :: Assertion
testAdder3 = testAdder adder'' "3"

tests :: [Test]
tests = TestCase <$> [testAdder1, testAdder2, testAdder3]

main :: IO Counts
main = runTestTT $ TestList tests
