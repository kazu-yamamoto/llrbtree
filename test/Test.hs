module Main where

import Data.RBTree.LL
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2

tests :: [Test]
tests = [ testGroup "Property Test" [
               testProperty "fromList"             prop_fromList
             ]
        ]

prop_fromList :: [Int] -> Bool
prop_fromList xs = valid $ fromList xs

main :: IO ()
main = defaultMain tests
