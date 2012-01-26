{-# LANGUAGE CPP, TemplateHaskell #-}

module Main where

import qualified Data.List as L
import Data.Set.BUSplay
import Prelude hiding (minimum)

import Test.Framework.TH.Prime
import Test.Framework.Providers.DocTest
import Test.Framework.Providers.QuickCheck2

main :: IO ()
main = $(defaultMainGenerator)

doc_test :: DocTests
doc_test = docTest ["../Data/Set/BUSplay.hs"] ["-i.."]

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
prop_member (x:xs) = mem
  where
    (mem, _) = member x t
    t = fromList (x:xs)

-- prop_delete :: [Int] -> Bool
-- prop_delete [] = True
-- prop_delete xs = valid t'
--   where
--     t = fromList xs
--     n = length xs `div` 2
--     t' = delete (xs !! n) t

-- prop_deleteRoot :: [Int] -> Bool
-- prop_deleteRoot [] = True
-- prop_deleteRoot xxs@(x:_) = valid t'
--   where
--     t = fromList xxs
--     t' = delete x t

-- prop_deleteLeaf :: [Int] -> Bool
-- prop_deleteLeaf [] = True
-- prop_deleteLeaf xs = valid t'
--   where
--     t = fromList xs
--     t' = delete (last xs) t

-- prop_deleteNon :: [Int] -> Int -> Bool
-- prop_deleteNon [] _ = True
-- prop_deleteNon xs x = valid t'
--   where
--     t = fromList xs
--     t' = delete x t

-- prop_deleteModel :: [Int] -> Bool
-- prop_deleteModel [] = True
-- prop_deleteModel xxs@(x:xs) = ys == zs
--   where
--     t = fromList xxs
--     t' = delete x t
--     ys = toList t'
--     zs = L.delete x . L.nub . L.sort $ xs

prop_deleteMin :: [Int] -> Bool
prop_deleteMin [] = True
prop_deleteMin xs = valid t'
  where
    t = fromList xs
    (_, t') = deleteMin t

prop_deleteMin2 :: [Int] -> Bool
prop_deleteMin2 [] = True
prop_deleteMin2 xs = ys == zs
  where
    t = fromList xs
    (_,t') = deleteMin t
    ys = toList t'
    zs = tail . L.nub . L.sort $ xs

-- prop_deleteMax :: [Int] -> Bool
-- prop_deleteMax [] = True
-- prop_deleteMax xs = valid t'
--   where
--     t = fromList xs
--     t' = deleteMax t

-- prop_deleteMax2 :: [Int] -> Bool
-- prop_deleteMax2 [] = True
-- prop_deleteMax2 xs = ys == zs
--   where
--     t = fromList xs
--     t' = deleteMax t
--     ys = reverse . toList $ t'
--     zs = tail . L.nub . L.sortBy (flip compare) $ xs

prop_union :: [Int] -> [Int] -> Bool
prop_union xs ys = valid $ union (fromList xs) (fromList ys)

prop_unionModel :: [Int] -> [Int] -> Bool
prop_unionModel xs ys = zs == zs'
  where
    zs = L.nub $ L.sort $ L.union xs ys
    zs' = toList $ union (fromList xs) (fromList ys)

-- prop_merge :: [Int] -> Bool
-- prop_merge [] = True
-- prop_merge (x:xs) = valid $ merge (fromList ys) (fromList zs)
--   where
--     ys = filter (<x) xs
--     zs = filter (>x) xs

prop_minimum :: [Int] -> Bool
prop_minimum [] = True
prop_minimum xs = minimum t == (m, t')
  where
    t = fromList xs
    m = L.minimum xs
    (_, t') = member m t
