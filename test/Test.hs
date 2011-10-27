module Main where

import qualified Data.List as L (delete)
import Data.List hiding (delete)
import Data.RBTree.LL
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2

tests :: [Test]
tests = [ testGroup "Property Test" [
               testProperty "fromList"           prop_fromList
             , testProperty "toList"             prop_toList
             , testProperty "member"             prop_member
               {-
             , testProperty "delete"             prop_delete
             , testProperty "delete2"            prop_delete2
-}
             , testProperty "deleteMin"          prop_deleteMin
             , testProperty "deleteMin2"         prop_deleteMin2
             , testProperty "deleteMax"          prop_deleteMax
             , testProperty "deleteMax2"         prop_deleteMax2
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

prop_member :: [Int] -> Bool
prop_member [] = True
prop_member (x:xs) = member x t
  where
    t = fromList (x:xs)

{-
prop_delete :: [Int] -> Bool
prop_delete [] = True
prop_delete (x:xs) = valid t'
  where
    t = fromList (x:xs)
    t' = delete x t

prop_delete2 :: [Int] -> Bool
prop_delete2 [] = True
prop_delete2 (x:xs) = ys == zs
  where
    t = fromList (x:xs)
    t' = delete x t
    ys = toList t'
    zs = L.delete x . nub . sort $ xs
-}

prop_deleteMin :: [Int] -> Bool
prop_deleteMin [] = True
prop_deleteMin xs = valid t'
  where
    t = fromList xs
    t' = deleteMin t

prop_deleteMin2 :: [Int] -> Bool
prop_deleteMin2 [] = True
prop_deleteMin2 xs = ys == zs
  where
    t = fromList xs
    t' = deleteMin t
    ys = toList t'
    zs = tail . nub . sort $ xs

prop_deleteMax :: [Int] -> Bool
prop_deleteMax [] = True
prop_deleteMax xs = valid t'
  where
    t = fromList xs
    t' = deleteMax t

prop_deleteMax2 :: [Int] -> Bool
prop_deleteMax2 [] = True
prop_deleteMax2 xs = ys == zs
  where
    t = fromList xs
    t' = deleteMax t
    ys = reverse . toList $ t'
    zs = tail . nub . sortBy (flip compare) $ xs

main :: IO ()
main = defaultMain tests
