module Data.RBTree (
    RBTree(..)
  , Color(..)
  , empty
  , insert
  , fromList
  , toList
  , member
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
unbalancedL R (B t1 x1 t2) x2 t3 = (balanceL (R t1 x1 t2) x2 t3, False)
unbalancedL B (B t1 x1 t2) x2 t3 = (balanceL (R t1 x1 t2) x2 t3, True)
unbalancedL B (R t1 x1 (B t2 x2 t3)) x3 t4 = (B t1 x1 (balanceL (R t2 x2 t3) x3 t4), False)
unbalancedL _ = error "unbalancedL"

unbalancedR :: RBTree a -> (RBTree a, Bool)
unbalancedR R t1 x1 (B t2 x2 t3) = (balanceR t1 x1 (R t2 x2 t3), False)
unbalancedR B t1 x1 (B t2 x2 t3) = (balanceR t1 x1 (R t2 x2 t3), True)
unbalancedR B t1 x1 (R (B t2 x2 t3) x3 t4) = (B (balanceR t1 x1 (R t2 x2 t3)) x3 t4, False)
unbalancedR _ = error "unbalancedR"

----------------------------------------------------------------

{-
deleteMin :: RBTree a -> RBTree a
deleteMin t = t'
  where
    (t', _, _) = deleteMin' t

deleteMin' :: RBTree a -> (RBTree a, a, Bool)
deleteMin' L = error "deleteMin'"
deleteMin' (B L x L) = (L, x, True)
deleteMin' (B L x (R l y r)) = (B l y r, x, False)
deleteMin' (B L _ (B _ _ _)) = error "deleteMin'"
deleteMin' (R L x r) = (r, x, False)
deleteMin' (B l x r) = if d then
                          let (t',d') = unbalancedR t in (t',m,d')
                       else
                          (t, m, False)
  where
    (l',m,d) = deleteMin' l
    t = B l' x r
deleteMin' (R l x r) = if d then
                          let (t',d') = unbalancedR t in (t',m,d')
                       else
                          (t, m, False)
  where
    (l',m,d) = deleteMin' l
    t = R l' x r

-}