

module Main where


import Test.Tasty
-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC
-- import Test.Tasty.HUnit as HU
-- import Test.Tasty.Golden as TG


tests :: TestTree
tests = testGroup "orchestrate"
    [
    ]

main :: IO ()
main = defaultMain tests

