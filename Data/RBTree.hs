module Data.RBTree (
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
valid = isBalanced

----------------------------------------------------------------

fromList :: Ord a => [a] -> RBTree a
fromList = foldl' (flip insert) empty

----------------------------------------------------------------
-- Chris Okasaki
--

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
balanceL B (Fork R a x (Fork R b y c)) z d =
    Fork R (Fork B a x b) y (Fork B c z d)
balanceL k a x b = Fork k a x b

balanceR :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balanceR B a x (Fork R b y (Fork R c z d)) =
    Fork R (Fork B a x b) y (Fork B c z d)
balanceR B a x (Fork R (Fork R b y c) z d) =
    Fork R (Fork B a x b) y (Fork B c z d)
balanceR k a x b = Fork k a x b

----------------------------------------------------------------

unbalancedL :: Color -> RBTree a -> a -> RBTree a -> (RBTree a, Bool)
unbalancedL R (Fork B t1 x1 t2) x2 t3 = (balanceL B (Fork R t1 x1 t2) x2 t3, False)
unbalancedL B (Fork B t1 x1 t2) x2 t3 = (balanceL B (Fork R t1 x1 t2) x2 t3, True)
unbalancedL B (Fork R t1 x1 (Fork B t2 x2 t3)) x3 t4 = (Fork B t1 x1 (balanceL B (Fork R t2 x2 t3) x3 t4), False)
unbalancedL _ _ _ _ = error "unbalancedL"

unbalancedR :: Color -> RBTree a -> a -> RBTree a -> (RBTree a, Bool)
unbalancedR c t1 x1 (Fork B t2 x2 t3) = (balanceR B t1 x1 (Fork R t2 x2 t3), c == B)
unbalancedR B t1 x1 (Fork R (Fork B t2 x2 t3) x3 t4) = (Fork B (balanceR B t1 x1 (Fork R t2 x2 t3)) x3 t4, False)
unbalancedR _ _ _ _ = error "unbalancedR"

----------------------------------------------------------------

deleteMin :: RBTree a -> RBTree a
deleteMin t = t'
  where
    (t', _, _) = deleteMin' t

deleteMin' :: RBTree a -> (RBTree a, a, Bool)
deleteMin' Leaf = error "deleteMin'"
deleteMin' (Fork B Leaf x Leaf) = (Leaf, x, True)
deleteMin' (Fork B Leaf x (Fork R l y r)) = (Fork B l y r, x, False)
deleteMin' (Fork B Leaf _ (Fork B _ _ _)) = error "deleteMin'"
deleteMin' (Fork R Leaf x r) = (r, x, False)
deleteMin' (Fork c l x r) = if d then
                                (t',m,d')
                            else
                                (Fork c l' x r, m, False)
  where
    (l',m,d) = deleteMin' l
    (t',d') = unbalancedR c l' x r
