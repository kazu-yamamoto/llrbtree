module Main where

import Data.RBTree.LL
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2

tests :: [Test]
tests = [ testGroup "Property Test" [
               testProperty "fromList"             prop_fromList
             , testProperty "toList"             prop_toList
             ]
        ]

prop_fromList :: [Int] -> Bool
prop_fromList xs = valid $ fromList xs

prop_toList :: [Int] -> Bool
prop_toList xs = ordered ys
  where
    ys = toList . fromList $ xs
    ordered (x:y:xys) = x <= y && ordered (y:xys)
    ordered _         = True

main :: IO ()
main = defaultMain tests
