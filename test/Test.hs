{-# LANGUAGE CPP #-}

module Main where

import qualified Data.List as L (delete)
import Data.List hiding (delete)
#if METHOD == 1
import Data.RBTree
#else
import Data.RBTree.LL
#endif
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2

tests :: [Test]
tests = [ testGroup "Property Test" [
               testProperty "fromList"           prop_fromList
             , testProperty "toList"             prop_toList
             , testProperty "member"             prop_member
             , testProperty "delete"             prop_delete
             , testProperty "deleteRoot"         prop_deleteRoot
             , testProperty "deleteLeaf"         prop_deleteLeaf
             , testProperty "deleteNon"          prop_deleteNon
             , testProperty "delete2"            prop_delete2
             , testProperty "deleteMin"          prop_deleteMin
             , testProperty "deleteMin2"         prop_deleteMin2
#if METHOD != 1
             , testProperty "deleteMax"          prop_deleteMax
             , testProperty "deleteMax2"         prop_deleteMax2
#endif
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

prop_deleteRoot :: [Int] -> Bool
prop_deleteRoot [] = True
prop_deleteRoot xxs@(x:_) = valid t'
  where
    t = fromList xxs
    t' = delete x t

prop_deleteLeaf :: [Int] -> Bool
prop_deleteLeaf [] = True
prop_deleteLeaf xs = valid t'
  where
    t = fromList xs
    t' = delete (last xs) t

prop_deleteNon :: [Int] -> Int -> Bool
prop_deleteNon [] _ = True
prop_deleteNon xs x = valid t'
  where
    t = fromList xs
    t' = delete x t

prop_delete :: [Int] -> Bool
prop_delete [] = True
prop_delete xs = valid t'
  where
    t = fromList xs
    n = length xs `div` 2
    t' = delete (xs !! n) t

prop_delete2 :: [Int] -> Bool
prop_delete2 [] = True
prop_delete2 xxs@(x:xs) = ys == zs
  where
    t = fromList xxs
    t' = delete x t
    ys = toList t'
    zs = L.delete x . nub . sort $ xs

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

#if METHOD != 1
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
#endif

main :: IO ()
main = defaultMain tests
