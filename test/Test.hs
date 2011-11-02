{-# LANGUAGE CPP #-}

module Main where

import qualified Data.List as L
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
             , testProperty "deleteMax"          prop_deleteMax
             , testProperty "deleteMax2"         prop_deleteMax2
#if METHOD != 1
             , testProperty "join"               prop_join
             , testProperty "merge"              prop_merge
             , testProperty "union"              prop_union
             , testProperty "unionModel"         prop_unionModel
             , testProperty "intersection"       prop_intersection
             , testProperty "intersectionModel"  prop_intersectionModel
             , testProperty "difference"         prop_difference
             , testProperty "differenceModel"    prop_differenceModel
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
    zs = L.delete x . L.nub . L.sort $ xs

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
    zs = tail . L.nub . L.sort $ xs

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
    zs = tail . L.nub . L.sortBy (flip compare) $ xs

#if METHOD != 1
prop_join :: [Int] -> Bool
prop_join [] = True
prop_join (x:xs) = valid $ join (fromList ys) x (fromList zs)
  where
    ys = filter (<x) xs
    zs = filter (>x) xs

prop_merge :: [Int] -> Bool
prop_merge [] = True
prop_merge (x:xs) = valid $ merge (fromList ys) (fromList zs)
  where
    ys = filter (<x) xs
    zs = filter (>x) xs

prop_union :: [Int] -> [Int] -> Bool
prop_union xs ys = valid $ union (fromList xs) (fromList ys)

prop_unionModel :: [Int] -> [Int] -> Bool
prop_unionModel xs ys = zs == zs'
  where
    zs = L.nub $ L.sort $ L.union xs ys
    zs' = toList $ union (fromList xs) (fromList ys)

prop_intersection :: [Int] -> [Int] -> Bool
prop_intersection xs ys = valid $ intersection (fromList xs) (fromList ys)

prop_intersectionModel :: [Int] -> [Int] -> Bool
prop_intersectionModel xs ys = zs == zs'
  where
    zs = L.nub $ L.sort $ L.intersect xs ys
    zs' = toList $ intersection (fromList xs) (fromList ys)

prop_difference :: [Int] -> [Int] -> Bool
prop_difference xs ys = valid $ difference (fromList xs) (fromList ys)

prop_differenceModel :: [Int] -> [Int] -> Bool
prop_differenceModel xs ys = zs == zs'
  where
    zs = L.sort $ L.nub xs L.\\ ys
    zs' = toList $ difference (fromList xs) (fromList ys)
#endif

main :: IO ()
main = defaultMain tests
