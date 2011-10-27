module Data.RBTree.LL (
    RBTree(..)
  , Color(..)
  , empty
  , insert
  , fromList
  , toList
  , member
  , deleteMin
  , deleteMax
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

deleteMin :: RBTree a -> RBTree a
deleteMin t = case deleteMin' (turnR t) of
    Leaf -> Leaf
    t'   -> turnB t'

deleteMin' :: RBTree a -> RBTree a
deleteMin' (Fork R Leaf _ Leaf) = Leaf
deleteMin' (Fork R l x r)
  | isBB && isBR = Fork R (Fork B (deleteMin' (turnR l)) x b) y (Fork B c z d)
  | isBB         = balanceR B (deleteMin' (turnR l)) x (turnR r)
  where
    isBB = isBlackLeftBlack l
    isBR = isBlackLeftRed r
    Fork B (Fork R b y c) z d = r
deleteMin' (Fork k l x r) = Fork k (deleteMin' l) x r
deleteMin' _ = error "deleteMin'"

----------------------------------------------------------------

deleteMax :: RBTree a -> RBTree a
deleteMax t = case deleteMax' (turnR t) of
    Leaf -> Leaf
    t'   -> turnB t'

deleteMax' :: RBTree a -> RBTree a
deleteMax' (Fork R Leaf _ Leaf) = Leaf
deleteMax' (Fork k (Fork R a x b) y c) = balanceR k a x (deleteMax' (Fork R b y c))
deleteMax' (Fork R l y r)
  | isBB && isBR = Fork R (turnB a) x (balanceR B b y (deleteMax' (turnR r)))
  | isBB         = balanceR B (turnR l) y (deleteMax' (turnR r))
  where
    isBB = isBlackLeftBlack r
    isBR = isBlackLeftRed l
    Fork B a@(Fork R _ _ _) x b = l
deleteMax' (Fork k l x r) = Fork k l x (deleteMax' r)
deleteMax' _ = error "deleteMax'"

