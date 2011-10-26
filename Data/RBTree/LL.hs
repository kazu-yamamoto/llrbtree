module Data.RBTree.LL (
    RBTree(..)
  , Color(..)
  , empty
  , insert
  , fromList
  , toList
  , member
  , deleteMin
  , valid
  ) where

import Data.List (foldl')
import Data.RBTree.Internal

----------------------------------------------------------------

valid :: RBTree a -> Bool
valid t = isBalanced t && isLeftLean t

isLeftLean :: RBTree a -> Bool
isLeftLean Leaf = True
isLeftLean (Fork B _ _ (Fork R _ _ _)) = False -- right only and both!
isLeftLean (Fork _ r _ l) = isLeftLean r && isLeftLean l

----------------------------------------------------------------

fromList :: Ord a => [a] -> RBTree a
fromList = foldl' (flip insert) empty

----------------------------------------------------------------

insert :: Ord a => a -> RBTree a -> RBTree a
insert a b = Fork B d e f
  where
    Fork _ d e f = ins a b
    ins x Leaf = Fork R Leaf x Leaf
    ins x t@(Fork c l y r) = case compare x y of
        LT -> balanceL c (ins x l) y r
        GT -> balanceR c l y (ins x r)
        EQ -> t

balanceL :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balanceL B (Fork R (Fork R a x b) y c) z d =
    Fork R (Fork B a x b) y (Fork B c z d)
balanceL k a x b = Fork k a x b

balanceR :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balanceR B (Fork R a x b) y (Fork R c z d) =
    Fork R (Fork B a x b) y (Fork B c z d)
-- x is Black since Red eliminated by the case above
-- x is either Fork or Leaf
balanceR k x y (Fork R c z d) =
    Fork k (Fork R x y c) z d
balanceR k a x b = Fork k a x b

----------------------------------------------------------------

turnR :: RBTree a -> RBTree a
turnR Leaf           = error "turnR"
turnR (Fork _ l x r) = Fork R l x r

turnB :: RBTree a -> RBTree a
turnB Leaf           = error "turnB"
turnB (Fork _ l x r) = Fork B l x r

deleteMin :: RBTree a -> RBTree a
deleteMin t = case deleteMin' (turnR t) of
    Leaf -> Leaf
    t'   -> turnB t'

deleteMin' :: RBTree a -> RBTree a
deleteMin' (Fork R Leaf _ Leaf) = Leaf
deleteMin' (Fork R a@(Fork B (Fork B _ _ _) _ _) x (Fork B (Fork R b y c) z d)) = Fork R (Fork B (deleteMin' (turnR a)) x b) y (Fork B c z d)
deleteMin' (Fork R a@(Fork B Leaf _ _) x (Fork B (Fork R b y c) z d)) = Fork R (Fork B (deleteMin' (turnR a)) x b) y (Fork B c z d)
deleteMin' (Fork R r@(Fork B (Fork B _ _ _) _ _) x l@(Fork B _ _ _)) = balanceR B (deleteMin' (turnR r)) x (turnR l)
deleteMin' (Fork R r@(Fork B Leaf _ _) x l@(Fork B _ _ _)) = balanceR B (deleteMin' (turnR r)) x (turnR l)
deleteMin' (Fork c l x r) = Fork c (deleteMin' l) x r
deleteMin' _ = error "deleteMin'"
