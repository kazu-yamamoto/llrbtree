{-# LANGUAGE CPP #-}

module Main where

import Random
#if METHOD == 1
import Data.RBTree
#else
import Data.RBTree.LL
#endif
import Progression.Main
import Criterion.Main (bench, bgroup, nf)
import Control.DeepSeq
import Prelude hiding (lookup)

seed :: Int
seed = 12345

main :: IO ()
main = do
    let !i1 = let l = [1..  1000] :: [Int] in l `deepseq` l
        !i2 = let l = [1.. 10000] :: [Int] in l `deepseq` l
        !i3 = let l = [1..100000] :: [Int] in l `deepseq` l
        !d1 = let l = [  1000,  999..1] :: [Int] in l `deepseq` l
        !d2 = let l = [ 10000, 9999..1] :: [Int] in l `deepseq` l
        !d3 = let l = [100000,99999..1] :: [Int] in l `deepseq` l
        !r1 = (take   1000 . randoms . mkStdGen $ seed) :: [Int]
        !r2 = (take  10000 . randoms . mkStdGen $ seed) :: [Int]
        !r3 = (take 100000 . randoms . mkStdGen $ seed) :: [Int]
    defaultMain $
        bgroup "" [
             bench "inc 10^3" $ nf fromList i1
           , bench "inc 10^4" $ nf fromList i2
           , bench "inc 10^5" $ nf fromList i3
           , bench "dec 10^3" $ nf fromList d1
           , bench "dec 10^4" $ nf fromList d2
           , bench "dec 10^5" $ nf fromList d3
           , bench "rnd 10^3" $ nf fromList r1
           , bench "rnd 10^4" $ nf fromList r2
           , bench "rnd 10^5" $ nf fromList r3
           ]

instance NFData a => NFData (RBTree a)
