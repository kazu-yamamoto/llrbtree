{-# LANGUAGE CPP, TemplateHaskell #-}

module Main where

import qualified Data.List as L
#if   METHOD == 1
import Data.Heap.Splay
#elif METHOD == 2
import Data.Heap.Skew
#elif METHOD == 3
import Data.Heap.Binominal
#elif METHOD == 4
import Data.Heap.Leftist
#else
import Data.Heap.Splay
#endif

import Test.Framework.TH.Prime
import Test.Framework.Providers.DocTest.Prime
import Test.Framework.Providers.QuickCheck2

main :: IO ()
main = $(defaultMainGenerator)

doc_test :: DocTests
#if   METHOD == 1
doc_test = docTest ["../Data/Heap/Splay.hs"] ["-i.."]
#elif METHOD == 2
doc_test = docTest ["../Data/Heap/Skew.hs"] ["-i.."]
#elif METHOD == 3
doc_test = docTest ["../Data/Heap/Binominal.hs"] ["-i.."]
#elif METHOD == 4
doc_test = docTest ["../Data/Heap/Leftist.hs"] ["-i.."]
#else
doc_test = docTest ["../Data/Heap/Splay.hs"] ["-i.."]
#endif

prop_fromList :: [Int] -> Bool
prop_fromList xs = valid $ fromList xs

prop_toList :: [Int] -> Bool
prop_toList xs = length (toList (fromList xs)) == length xs

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
    ys = heapSort t'
    zs = tail . L.sort $ xs

prop_merge :: [Int] -> Bool
prop_merge [] = True
prop_merge (x:xs) = valid $ merge (fromList ys) (fromList zs)
  where
    ys = filter (<x) xs
    zs = filter (>x) xs
