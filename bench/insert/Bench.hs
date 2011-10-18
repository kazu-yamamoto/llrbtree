{-# LANGUAGE CPP, BangPatterns #-}

module Main where

#if METHOD == 1
import Data.RBTree
#else
import Data.RBTree.LL
#endif
import Control.DeepSeq
import Criterion.Main (bench, bgroup, nf)
import Prelude hiding (lookup)
import Progression.Main
import System.Random

seed :: Int
seed = 54321

ensure :: [Int] -> IO [Int]
ensure xs = xs `deepseq` return xs

genRandom :: Int -> [Int]
genRandom n = take n . randoms . mkStdGen $ seed

main :: IO ()
main = do
    !i1 <- ensure [1..  10000]
    !i2 <- ensure [1.. 100000]
    !i3 <- ensure [1..1000000]
    !d1 <- ensure [  10000,  9999..1]
    !d2 <- ensure [ 100000, 99999..1]
    !d3 <- ensure [1000000,999999..1]
    !r1 <- ensure $ genRandom   10000
    !r2 <- ensure $ genRandom  100000
    !r3 <- ensure $ genRandom 1000000
    defaultMain $
        bgroup "" [
             bench "inc 10^4" $ nf fromList i1
           , bench "inc 10^5" $ nf fromList i2
           , bench "inc 10^6" $ nf fromList i3
           , bench "dec 10^4" $ nf fromList d1
           , bench "dec 10^5" $ nf fromList d2
           , bench "dec 10^6" $ nf fromList d3
           , bench "rnd 10^4" $ nf fromList r1
           , bench "rnd 10^5" $ nf fromList r2
           , bench "rnd 10^6" $ nf fromList r3
           ]

instance NFData a => NFData (RBTree a)
