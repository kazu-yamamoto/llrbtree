{-# LANGUAGE CPP, TemplateHaskell #-}

module Main where

import qualified Data.List as L
#if   METHOD == 1
import Data.Set.RBTree
#elif METHOD == 2
import Data.Set.LLRBTree
#elif METHOD == 3
import Data.Set.WBTree
#else
import Data.Set.LLRBTree
#endif

import Test.Framework.TH.Prime
import Test.Framework.Providers.DocTest
import Test.Framework.Providers.QuickCheck2

main :: IO ()
main = $(defaultMainGenerator)

doc_test :: DocTests
#if   METHOD == 1
doc_test = docTest ["../Data/Set/RBTree.hs"] ["-i.."]
#elif METHOD == 2
doc_test = docTest ["../Data/Set/LLRBTree.hs"] ["-i.."]
#elif METHOD == 3
doc_test = docTest ["../Data/Set/WBTree.hs"] ["-i.."]
#else
doc_test = docTest ["../Data/Set/LLRBTree.hs"] ["-i.."]
#endif

prop_fromList :: [Int] -> Bool
prop_fromList xs = valid $ fromList xs

prop_toList :: [Int] -> Bool
prop_toList xs = ordered ys
  where
    ys = toList . fromList $ xs
    ordered (x:y:xys) = x <= y && ordered (y:xys)
    ordered _         = True

prop_member :: [Int] -> Bool
prop_member []        = member (1::Int) (fromList []) == False
prop_member xxs@(x:_) = member x t
  where
    t = fromList xxs

prop_memberModel :: Int -> [Int] -> Bool
prop_memberModel x xs = member x t == elem x xs
  where
    t = fromList xs

prop_delete :: [Int] -> Bool
prop_delete [] = True
prop_delete xs = valid t'
  where
    t = fromList xs
    n = length xs `div` 2
    t' = delete (xs !! n) t

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

prop_deleteModel :: [Int] -> Bool
prop_deleteModel [] = True
prop_deleteModel xxs@(x:xs) = ys == zs
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

prop_split :: [Int] -> Bool
prop_split [] = True
prop_split xs = valid lt && valid gt
  where
    t = fromList xs
    n = length xs `div` 2
    (lt,gt) = split (xs !! n) t

prop_splitRoot :: [Int] -> Bool
prop_splitRoot [] = True
prop_splitRoot xxs@(x:_) = valid lt && valid gt
  where
    t = fromList xxs
    (lt,gt) = split x t

prop_splitLeaf :: [Int] -> Bool
prop_splitLeaf [] = True
prop_splitLeaf xs = valid lt && valid gt
  where
    t = fromList xs
    (lt,gt) = split (last xs) t

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
